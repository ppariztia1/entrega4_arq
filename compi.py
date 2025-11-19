import re
DECLARED = {"a","b","c","d","e","f","g","result","error","zero"}
IMMEDIATE_ZERO = True   # usar MOV A,0 en vez de MOV A,(zero)

# ================================================
#                   LEXER
# ================================================

TOKEN_SPEC = [
    ("NUMBER", r"\d+"),
    ("ID",     r"[a-zA-Z_]\w*"),
    ("OP",     r"[+\-*/%]"),
    ("LPAREN", r"\("),
    ("RPAREN", r"\)"),
    ("COMMA",  r","),
    ("EQ",     r"="),
    ("SKIP",   r"[ \t]+"),
]

TOK_REGEX = "|".join(f"(?P<{name}>{regex})" for name,regex in TOKEN_SPEC)
MASTER_RE = re.compile(TOK_REGEX)

class Token:
    def _init_(self, typ, val):
        self.type = typ
        self.val = val

def lex(s):
    tokens = []
    for m in MASTER_RE.finditer(s):
        typ = m.lastgroup
        if typ != "SKIP":
            tokens.append(Token(typ, m.group(typ)))
    return tokens

# ================================================
#                   PARSER
# ================================================

class Parser:
    def _init_(self, tokens):
        self.tokens = tokens
        self.pos = 0

    def cur(self):
        return self.tokens[self.pos] if self.pos < len(self.tokens) else Token("EOF","")

    def eat(self, ttype=None, val=None):
        tok = self.cur()
        if ttype and tok.type != ttype:
            raise ValueError(f"Esperaba tipo {ttype}, llegó {tok}")
        if val and tok.val != val:
            raise ValueError(f"Esperaba {val}, llegó {tok.val}")
        self.pos += 1
        return tok

    def parse_assignment(self):
        left = self.eat("ID").val
        self.eat("EQ")
        expr = self.parse_expr()
        return left, expr

    def parse_expr(self):
        node = self.parse_term()
        while self.cur().type=="OP" and self.cur().val in ("+","-"):
            op = self.eat("OP").val
            right = self.parse_term()
            node = ("binop",op,node,right)
        return node

    def parse_term(self):
        node = self.parse_factor()
        while self.cur().type=="OP" and self.cur().val in ("*","/","%"):
            op = self.eat("OP").val
            right = self.parse_factor()
            node = ("binop",op,node,right)
        return node

    def parse_factor(self):
        tok = self.cur()

        # unary -
        if tok.type=="OP" and tok.val=="-":
            self.eat("OP")
            return ("neg", self.parse_factor())

        # variable or function
        if tok.type=="ID":
            name = self.eat("ID").val
            if self.cur().type == "LPAREN":
                self.eat("LPAREN")
                args=[]
                if name.lower() in ("max","min"):
                    args.append(self.parse_expr())
                    self.eat("COMMA")
                    args.append(self.parse_expr())
                elif name.lower()=="abs":
                    args.append(self.parse_expr())
                else:
                    raise ValueError(f"Función desconocida {name}")
                self.eat("RPAREN")
                return ("func",name.lower(),args)
            return ("var",name)

        # parenthesis
        if tok.type=="LPAREN":
            self.eat("LPAREN")
            node = self.parse_expr()
            self.eat("RPAREN")
            return node

        # constant 0
        if tok.type=="NUMBER":
            num = int(self.eat("NUMBER").val)
            if num!=0:
                raise ValueError("Solo se acepta constante 0")
            return ("const0",)

        raise ValueError(f"Token inesperado {tok}")

# ================================================
#        CODE GENERATOR COMPATIBLE ASUA
# ================================================

class CodeGen:
    def _init_(self):
        self.code=[]
        self.reads=0
        self.writes=0
        self.temp_counter=0
        self.label_counter=0
        self.error_label=self.new_label()
        self.end_label=self.new_label()
        self.used_vars = set()
        self.temps = set()


    def emit(self,line):
        self.code.append(line)

    def mem_read(self):
        self.reads+=1
    def mem_write(self):
        self.writes+=1

    def new_temp(self):
        self.temp_counter += 1
        name = f"t{self.temp_counter}"
        self.temps.add(name)
        return name



    def new_label(self):
        self.label_counter+=1
        return f"L{self.label_counter}"

    # ---------------------------
    # helpers A/B/memory
    # ---------------------------

    def loadA(self,var):
        self.emit(f"MOV A,({var})")
        self.mem_read()

    def loadB(self,var):
        self.emit(f"MOV B,({var})")
        self.mem_read()

    def movA_immediate(self,val):
        self.emit(f"MOV A,{val}")

    def storeA(self,var):
        self.emit(f"MOV ({var}),A")
        self.mem_write()

    def store_zero(self,var):
        if IMMEDIATE_ZERO:
            self.emit("MOV A,0")
        else:
            self.emit("MOV A,(zero)"); self.mem_read()
        self.storeA(var)

    # ---------------------------
    # GENERATION
    # ---------------------------
    def gen(self,node):
        kind=node[0]

        if kind == "var":
            name = node[1]
            self.used_vars.add(name)
            return name


        if kind=="const0":
            t=self.new_temp()
            self.store_zero(t)
            return t

        if kind=="neg":
            loc=self.gen(node[1])
            t=self.new_temp()
            self.store_zero(t)
            self.loadA(t)
            self.loadB(loc)
            self.emit("SUB A,B")
            self.storeA(t)
            return t

        if kind=="binop":
            op,L,R=node[1],node[2],node[3]
            left=self.gen(L)
            right=self.gen(R)
            if op in ("+","-"):
                t=self.new_temp()
                self.loadA(left)
                self.loadB(right)
                self.emit("ADD A,B" if op=="+" else "SUB A,B")
                self.storeA(t)
                return t
            if op=="*":
                return self.gen_mul(left,right)
            if op=="/":
                return self.gen_div(left,right)
            if op=="%":
                return self.gen_mod(left,right)

        if kind=="func":
            fname=node[1]
            args=node[2]
            if fname=="max":
                return self.gen_max(args[0],args[1])
            if fname=="min":
                return self.gen_min(args[0],args[1])
            if fname=="abs":
                return self.gen_abs(args[0])

        raise ValueError(f"Nodo no manejado {node}")

    # ================================================
    #     MULTIPLICACIÓN: a * b (simple)
    # ================================================
    def gen_mul(self,L,R):
        m=self.new_temp()   # multiplicando
        n=self.new_temp()   # multiplicador
        acc=self.new_temp()
        res=self.new_temp()

        # m = L
        self.loadA(L)
        self.storeA(m)

        # n = R
        self.loadA(R)
        self.storeA(n)

        # acc=0
        self.store_zero(acc)

        Lstart=self.new_label()
        Lend=self.new_label()

        self.emit(f"{Lstart}:")
        self.loadA(n)
        self.emit("CMP A,0")
        self.emit(f"JEQ {Lend}")

        # acc = acc + m
        self.loadA(acc)
        self.loadB(m)
        self.emit("ADD A,B")
        self.storeA(acc)

        # n = n - 1
        self.loadA(n)
        self.movA_immediate(1)
        self.emit("SUB A,B")
        self.storeA(n)

        self.emit(f"JMP {Lstart}")

        self.emit(f"{Lend}:")
        self.loadA(acc)
        self.storeA(res)
        return res

    # ================================================
    #    DIVISIÓN: a / b (simples, positivos)
    # ================================================
    def gen_div(self,L,R):
        dividend=self.new_temp()
        divisor=self.new_temp()
        q=self.new_temp()
        res=self.new_temp()

        self.loadA(L)
        self.storeA(dividend)

        self.loadA(R)
        self.storeA(divisor)

        # divisor == 0 -> error
        self.loadA(divisor)
        self.emit("CMP A,0")
        self.emit(f"JEQ {self.error_label}")

        # q=0
        self.store_zero(q)

        Lstart=self.new_label()
        Lbody=self.new_label()
        Lend=self.new_label()

        self.emit(f"{Lstart}:")
        # if divisor <= dividend
        self.loadA(divisor)
        self.loadB(dividend)
        self.emit("CMP A,B")
        self.emit(f"JLE {Lbody}")
        self.emit(f"JMP {Lend}")

        self.emit(f"{Lbody}:")
        # dividend -= divisor
        self.loadA(dividend)
        self.loadB(divisor)
        self.emit("SUB A,B")
        self.storeA(dividend)

        # q += 1
        self.loadA(q)
        self.movA_immediate(1)
        self.emit("ADD A,B")
        self.storeA(q)

        self.emit(f"JMP {Lstart}")

        self.emit(f"{Lend}:")
        self.loadA(q)
        self.storeA(res)
        return res

    # ================================================
    #     MODULO: a % b
    # ================================================
    def gen_mod(self,L,R):
        dividend=self.new_temp()
        divisor=self.new_temp()
        res=self.new_temp()

        self.loadA(L)
        self.storeA(dividend)
        self.loadA(R)
        self.storeA(divisor)

        self.loadA(divisor)
        self.emit("CMP A,0")
        self.emit(f"JEQ {self.error_label}")

        Lstart=self.new_label()
        Lbody=self.new_label()
        Lend=self.new_label()

        self.emit(f"{Lstart}:")
        self.loadA(divisor)
        self.loadB(dividend)
        self.emit("CMP A,B")
        self.emit(f"JLE {Lbody}")
        self.emit(f"JMP {Lend}")

        self.emit(f"{Lbody}:")
        self.loadA(dividend)
        self.loadB(divisor)
        self.emit("SUB A,B")
        self.storeA(dividend)
        self.emit(f"JMP {Lstart}")

        self.emit(f"{Lend}:")
        self.loadA(dividend)
        self.storeA(res)
        return res

    # ================================================
    #     max(x,y)
    # ================================================
    def gen_max(self,X,Y):
        a=self.gen(X)
        b=self.gen(Y)
        t=self.new_temp()
        Ldone=self.new_label()

        self.loadA(a)
        self.loadB(b)
        self.emit("CMP A,B")
        self.emit(f"JGE {Ldone}")
        self.loadA(b)
        self.emit(f"{Ldone}:")
        self.storeA(t)
        return t

    # ================================================
    #     min(x,y)
    # ================================================
    def gen_min(self,X,Y):
        a=self.gen(X)
        b=self.gen(Y)
        t=self.new_temp()
        Ldone=self.new_label()

        self.loadA(a)
        self.loadB(b)
        self.emit("CMP A,B")
        self.emit(f"JLE {Ldone}")
        self.loadA(b)
        self.emit(f"{Ldone}:")
        self.storeA(t)
        return t

    # ================================================
    #     abs(x)
    # ================================================
    def gen_abs(self,X):
        x=self.gen(X)
        t=self.new_temp()
        Lok=self.new_label()

        self.loadA(x)
        self.emit("CMP A,0")
        self.emit(f"JGE {Lok}")

        # negativo -> A = -A
        self.emit("MOV B,A")
        if IMMEDIATE_ZERO:
            self.emit("MOV A,0")
        else:
            self.emit("MOV A,(zero)"); self.mem_read()
        self.emit("SUB A,B")

        self.emit(f"{Lok}:")
        self.storeA(t)
        return t

# ================================================
#  FUNCIÓN PRINCIPAL
# ================================================

def compile_to_asua(expr):
    tokens=lex(expr)
    p=Parser(tokens)
    lhs,ast=p.parse_assignment()

    if lhs!="result":
        raise ValueError("La asignación debe ser: result = ...")

    gen=CodeGen()
    final_loc=gen.gen(ast)

    # guardar resultado normal
    gen.loadA(final_loc)
    gen.emit("MOV (result),A")
    gen.mem_write()

    gen.emit(f"JMP {gen.end_label}")

    # rutina error
    gen.emit(f"{gen.error_label}:")
    gen.movA_immediate(1)
    gen.emit("MOV (error),A")
    gen.mem_write()
    if IMMEDIATE_ZERO:
        gen.movA_immediate(0)
    else:
        gen.loadA("zero")
    gen.emit("MOV (result),A")
    gen.mem_write()
    gen.emit(f"JMP {gen.end_label}")

    # fin
    gen.emit(f"{gen.end_label}:")
    gen.emit("HLT")

    stats={
        "lines":len(gen.code),
        "reads":gen.reads,
        "writes":gen.writes,
        "mem_accesses":gen.reads+gen.writes
    }
    # ---------------------------------------------------------
    # VALIDACIÓN DE VARIABLES NO DECLARADAS
    # ---------------------------------------------------------



    # ---------------------------------------------------------
    # VALIDACIÓN DE VARIABLES USADAS VS DECLARADAS
    # ---------------------------------------------------------
    
    valid = DECLARED.union(gen.temps)
    
    unknown = [v for v in gen.used_vars if v not in valid]
    
    if unknown:
        raise ValueError(f"Variables no declaradas: {', '.join(unknown)}")

    return gen.code,stats


if _name=="main_":
    expr=input("Expr: ")
    code,stats=compile_to_asua(expr)
    
    print("\nCODE:")
    for line in code:
        print(line)
    print("\n# Stats:",stats)