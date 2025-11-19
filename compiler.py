import re

IMMEDIATE_ZERO = True


# ============================================================
# LEXER
# ============================================================

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

MASTER_RE = re.compile(
    "|".join(f"(?P<{name}>{regex})" for name, regex in TOKEN_SPEC)
)


class Token:
    def __init__(self, typ, val):
        self.type = typ
        self.val = val


def lex(s):
    tokens = []
    for m in MASTER_RE.finditer(s):
        typ = m.lastgroup
        if typ != "SKIP":
            tokens.append(Token(typ, m.group(typ)))
    return tokens


# ============================================================
# PARSER
# ============================================================

class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.pos = 0

    def cur(self):
        return self.tokens[self.pos] if self.pos < len(self.tokens) else Token("EOF", "")

    def eat(self, typ=None, val=None):
        tok = self.cur()
        if typ and tok.type != typ:
            raise ValueError(f"Esperaba {typ}, llegó {tok}")
        if val and tok.val != val:
            raise ValueError(f"Esperaba '{val}', llegó '{tok.val}'")
        self.pos += 1
        return tok

    def parse_assignment(self):
        left = self.eat("ID").val
        self.eat("EQ")
        expr = self.parse_expr()
        return left, expr

    def parse_expr(self):
        node = self.parse_term()
        while self.cur().type == "OP" and self.cur().val in ("+", "-"):
            op = self.eat("OP").val
            node = ("binop", op, node, self.parse_term())
        return node

    def parse_term(self):
        node = self.parse_factor()
        while self.cur().type == "OP" and self.cur().val in ("*", "/", "%"):
            op = self.eat("OP").val
            node = ("binop", op, node, self.parse_factor())
        return node

    def parse_factor(self):
        tok = self.cur()

        if tok.type == "OP" and tok.val == "-":
            self.eat("OP")
            return ("neg", self.parse_factor())

        if tok.type == "ID":
            name = self.eat("ID").val
            if self.cur().type == "LPAREN":
                self.eat("LPAREN")
                args = []
                if name.lower() in ("max", "min"):
                    args.append(self.parse_expr())
                    self.eat("COMMA")
                    args.append(self.parse_expr())
                elif name.lower() == "abs":
                    args.append(self.parse_expr())
                else:
                    raise ValueError(f"Función no soportada: {name}")
                self.eat("RPAREN")
                return ("func", name.lower(), args)
            return ("var", name)

        if tok.type == "LPAREN":
            self.eat("LPAREN")
            node = self.parse_expr()
            self.eat("RPAREN")
            return node

        if tok.type == "NUMBER":
            num = int(self.eat("NUMBER").val)
            if num != 0:
                raise ValueError("Solo se permite constante 0")
            return ("const0",)

        raise ValueError(f"Token inesperado: {tok}")


# ============================================================
# AST SIMPLE OPT
# ============================================================

def simplify(node):
    kind = node[0]

    if kind in ("var", "const0"):
        return node

    if kind == "neg":
        return ("neg", simplify(node[1]))

    if kind == "func":
        return ("func", node[1], [simplify(a) for a in node[2]])

    if kind == "binop":
        op, L, R = node[1], simplify(node[2]), simplify(node[3])

        if op == "*":
            if L[0] == "const0" or R[0] == "const0":
                return ("const0",)

        if op == "+":
            if L[0] == "const0":
                return R
            if R[0] == "const0":
                return L

        if op == "-":
            if R[0] == "const0":
                return L
            if L[0] == "const0":
                return ("neg", R)

        return ("binop", op, L, R)

    return node


# ============================================================
# CODE GEN
# ============================================================

class CodeGen:
    def __init__(self):
        self.code = []
        self.reads = 0
        self.writes = 0
        self.temp_counter = 0
        self.label_counter = 0
        self.error_label = self.new_label()
        self.end_label = self.new_label()

    def emit(self, line):
        self.code.append(line)

    def mem_read(self):
        self.reads += 1

    def mem_write(self):
        self.writes += 1

    def new_temp(self):
        self.temp_counter += 1
        return f"t{self.temp_counter}"

    def new_label(self):
        self.label_counter += 1
        return f"L{self.label_counter}"

    def loadA(self, var):
        self.emit(f"MOV A,({var})")
        self.mem_read()

    def loadB(self, var):
        self.emit(f"MOV B,({var})")
        self.mem_read()

    def moveA_imm(self, val):
        self.emit(f"MOV A,{val}")

    def moveB_imm(self, val):
        self.emit(f"MOV B,{val}")

    def storeA(self, var):
        self.emit(f"MOV ({var}),A")
        self.mem_write()

    def store_zero(self, var):
        if IMMEDIATE_ZERO:
            self.moveA_imm(0)
        else:
            self.emit("MOV A,(zero)")
            self.mem_read()
        self.storeA(var)

    # ============================================================
    # GENERADOR PRINCIPAL DE EXPRESIONES
    # ============================================================
    def gen(self, node):
        kind = node[0]

        # variable
        if kind == "var":
            return node[1]

        # constante 0
        if kind == "const0":
            t = self.new_temp()
            self.store_zero(t)
            return t

        # negación
        if kind == "neg":
            loc = self.gen(node[1])
            t = self.new_temp()
            self.store_zero(t)
            self.loadA(t)
            self.loadB(loc)
            self.emit("SUB A,B")
            self.storeA(t)
            return t

        # funciones
        if kind == "func":
            fname = node[1]
            args = node[2]

            if fname == "max":
                return self.gen_max(args[0], args[1])
            if fname == "min":
                return self.gen_min(args[0], args[1])
            if fname == "abs":
                return self.gen_abs(args[0])

            raise ValueError("Función no soportada: " + fname)

        # operaciones binarias
        if kind == "binop":
            op, L, R = node[1], node[2], node[3]

            if op == "+":
                return self.gen_add(L, R)
            if op == "-":
                return self.gen_sub(L, R)
            if op == "*":
                return self.gen_mul(self.gen(L), self.gen(R))
            if op == "/":
                return self.gen_div(self.gen(L), self.gen(R))
            if op == "%":
                return self.gen_mod(self.gen(L), self.gen(R))

            raise ValueError("Operador no soportado: " + op)

        raise ValueError("Nodo AST no reconocido: " + str(node))

    # ============================================================
    # SUMA CON OVERFLOW
    # ============================================================
    def gen_add(self, L, R):
        l = self.gen(L)
        r = self.gen(R)

        t = self.new_temp()
        self.loadA(l)
        self.loadB(r)
        self.emit("ADD A,B")

        # overflow +127
        self.emit("CMP A,127")
        self.emit(f"JGT {self.error_label}")

        self.storeA(t)
        return t

    # ============================================================
    # RESTA CON OVERFLOW
    # ============================================================
    def gen_sub(self, L, R):
        l = self.gen(L)
        r = self.gen(R)

        t = self.new_temp()
        self.loadA(l)
        self.loadB(r)
        self.emit("SUB A,B")

        # overflow positivo
        self.emit("CMP A,127")
        self.emit(f"JGT {self.error_label}")

        self.storeA(t)
        return t


    # ============================================================
    # MULTIPLICACIÓN CON SIGNO + OVERFLOW CORRECTO
    # ============================================================

    def gen_mul(self, l, r):
        self.loadA(l)
        t_a = self.new_temp()
        self.storeA(t_a)

        self.loadA(r)
        t_b = self.new_temp()
        self.storeA(t_b)

        # signo
        t_sign = self.new_temp()
        self.store_zero(t_sign)

        # abs(a)
        La_pos = self.new_label()
        La_end = self.new_label()

        self.loadA(t_a)
        self.emit("CMP A,0")
        self.emit(f"JGE {La_pos}")

        self.moveA_imm(1)
        self.storeA(t_sign)

        self.loadA(t_a)
        self.emit("MOV B,A")
        self.moveA_imm(0)
        self.emit("SUB A,B")
        self.storeA(t_a)
        self.emit(f"JMP {La_end}")

        self.emit(f"{La_pos}:")
        self.emit(f"{La_end}:")

        # abs(b)
        Lb_pos = self.new_label()
        Lb_end = self.new_label()

        self.loadA(t_b)
        self.emit("CMP A,0")
        self.emit(f"JGE {Lb_pos}")

        tmp = self.new_temp()
        self.loadA(t_sign)
        self.storeA(tmp)

        self.moveA_imm(1)
        self.loadB(tmp)
        self.emit("SUB A,B")
        self.storeA(t_sign)

        self.loadA(t_b)
        self.emit("MOV B,A")
        self.moveA_imm(0)
        self.emit("SUB A,B")
        self.storeA(t_b)
        self.emit(f"JMP {Lb_end}")

        self.emit(f"{Lb_pos}:")
        self.emit(f"{Lb_end}:")

        # multiplicar positivos
        t_acc = self.new_temp()
        self.store_zero(t_acc)

        t_n = self.new_temp()
        self.loadA(t_b)
        self.storeA(t_n)

        Lloop = self.new_label()
        Ldone = self.new_label()

        self.emit(f"{Lloop}:")
        self.loadA(t_n)
        self.emit("CMP A,0")
        self.emit(f"JEQ {Ldone}")

        self.loadA(t_acc)
        self.loadB(t_a)
        self.emit("ADD A,B")

        # overflow positivo
        self.emit("CMP A,127")
        self.emit(f"JGT {self.error_label}")

        self.storeA(t_acc)

        self.loadA(t_n)
        self.moveB_imm(1)
        self.emit("SUB A,B")
        self.storeA(t_n)

        self.emit(f"JMP {Lloop}")

        self.emit(f"{Ldone}:")
        t_pos = self.new_temp()
        self.loadA(t_acc)
        self.storeA(t_pos)

        # aplicar signo
        Lpos = self.new_label()
        Lend = self.new_label()

        self.loadA(t_sign)
        self.emit("CMP A,0")
        self.emit(f"JEQ {Lpos}")

        # negativo: -pos
        self.loadA(t_pos)
        self.emit("MOV B,A")
        self.moveA_imm(0)
        self.emit("SUB A,B")

        # aquí NO hacer check A<128
        # solo overflow positivo
        self.emit("CMP A,127")
        self.emit(f"JGT {self.error_label}")

        t_final = self.new_temp()
        self.storeA(t_final)
        self.emit(f"JMP {Lend}")

        self.emit(f"{Lpos}:")
        t_final = self.new_temp()
        self.loadA(t_pos)
        self.storeA(t_final)

        self.emit(f"{Lend}:")
        return t_final

    # ============================================================
    # DIV, MOD, MAX, MIN, ABS
    # (NO CAMBIADOS)
    # ============================================================

    def gen_div(self, l, r):
        dividend = self.new_temp()
        divisor = self.new_temp()
        q = self.new_temp()
        res = self.new_temp()

        self.loadA(l)
        self.storeA(dividend)
        self.loadA(r)
        self.storeA(divisor)

        self.loadA(divisor)
        self.emit("CMP A,0")
        self.emit(f"JEQ {self.error_label}")

        self.store_zero(q)

        Lstart = self.new_label()
        Lbody = self.new_label()
        Lend = self.new_label()

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

        self.loadA(q)
        self.moveB_imm(1)
        self.emit("ADD A,B")
        self.storeA(q)

        self.emit(f"JMP {Lstart}")

        self.emit(f"{Lend}:")
        self.loadA(q)
        self.storeA(res)
        return res

    def gen_mod(self, l, r):
        dividend = self.new_temp()
        divisor = self.new_temp()
        res = self.new_temp()

        self.loadA(l)
        self.storeA(dividend)
        self.loadA(r)
        self.storeA(divisor)

        self.loadA(divisor)
        self.emit("CMP A,0")
        self.emit(f"JEQ {self.error_label}")

        Lstart = self.new_label()
        Lbody = self.new_label()
        Lend = self.new_label()

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

    def gen_max(self, X, Y):
        a = self.gen(X)
        b = self.gen(Y)
        t = self.new_temp()
        Ldone = self.new_label()

        self.loadA(a)
        self.loadB(b)
        self.emit("CMP A,B")
        self.emit(f"JGE {Ldone}")
        self.loadA(b)

        self.emit(f"{Ldone}:")
        self.storeA(t)
        return t

    def gen_min(self, X, Y):
        a = self.gen(X)
        b = self.gen(Y)
        t = self.new_temp()
        Ldone = self.new_label()

        self.loadA(a)
        self.loadB(b)
        self.emit("CMP A,B")
        self.emit(f"JLE {Ldone}")
        self.loadA(b)

        self.emit(f"{Ldone}:")
        self.storeA(t)
        return t

    def gen_abs(self, X):
        x = self.gen(X)
        t = self.new_temp()
        Lok = self.new_label()

        self.loadA(x)
        self.emit("CMP A,0")
        self.emit(f"JGE {Lok}")

        self.emit("MOV B,A")
        self.moveA_imm(0)
        self.emit("SUB A,B")

        self.emit(f"{Lok}:")
        self.storeA(t)
        return t


# ============================================================
# COMPILER MAIN
# ============================================================

def compile_to_asua(expr):
    tokens = lex(expr)
    p = Parser(tokens)
    lhs, ast = p.parse_assignment()

    if lhs != "result":
        raise ValueError("La expresión debe ser de la forma: result = ...")

    ast = simplify(ast)

    gen = CodeGen()
    final_loc = gen.gen(ast)


    gen.loadA(final_loc)
    gen.emit("MOV (result),A")
    gen.mem_write()
    gen.emit(f"JMP {gen.end_label}")

    # RUTINA ERROR FINAL (overflow / div0)
    gen.emit(f"{gen.error_label}:")
    gen.moveA_imm(1)
    gen.emit("MOV (error),A")
    gen.mem_write()
    gen.moveA_imm(0)
    gen.emit("MOV (result),A")
    gen.mem_write()
    gen.emit(f"JMP {gen.end_label}")

    gen.emit(f"{gen.end_label}:")
    gen.emit("HLT")

    stats = {
        "lines": len(gen.code),
        "reads": gen.reads,
        "writes": gen.writes,
        "mem_accesses": gen.reads + gen.writes,
    }
    return gen.code, stats


if __name__ == "__main__":
    expr = input("Expr: ")
    code, stats = compile_to_asua(expr)
    print("\nCODE:")
    for line in code:
        print(line)
    print("\n# Stats:", stats)