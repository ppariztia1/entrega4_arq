import re

IMMEDIATE_ZERO = True   # Si tu emulador NO acepta MOV A,0: pon False y usa (zero)

def tokenize(expr):
    expr = expr.replace('(', ' ( ').replace(')', ' ) ')
    return expr.split()

def flatten(rhs):
    # Devuelve lista de (signo, var) con signos propagados por paréntesis
    tokens = tokenize(rhs)
    sign_stack, pending, out = [1], 1, []
    for t in tokens:
        if t == '+': pending = 1
        elif t == '-': pending = -1
        elif t == '(':
            sign_stack.append(sign_stack[-1] * pending); pending = 1
        elif t == ')':
            sign_stack.pop()
        else:
            out.append((sign_stack[-1] * pending, t)); pending = 1
    return out

def compile_to_asua_with_stats(expr):
    lhs, rhs = [s.strip() for s in expr.split('=')]
    terms = flatten(rhs)

    code = []
    reads = writes = 0

    # elegir primer término positivo si existe
    pos_idx = next((i for i,(s,_) in enumerate(terms) if s == 1), None)

    if pos_idx is None:
        # todos negativos: arrancar en cero y restar
        if IMMEDIATE_ZERO:
            code.append("MOV A,0")          # 0 lecturas
        else:
            code.append("MOV A,(zero)"); reads += 1
        for _, var in terms:                 # todos con signo -1
            code.append(f"MOV B,(v_{var})"); reads += 1
            code.append("SUB A,B")
    else:
        # arrancar con primer positivo
        _, first = terms[pos_idx]
        code.append(f"MOV A,(v_{first})"); reads += 1
        for i, (sgn, var) in enumerate(terms):
            if i == pos_idx: continue
            code.append(f"MOV B,(v_{var})"); reads += 1
            code.append("ADD A,B" if sgn == 1 else "SUB A,B")

    code.append(f"MOV (result),A"); writes += 1
    code.append("HLT")

    lines = len(code)
    mem_accesses = reads + writes
    return code, {"lines": lines, "reads": reads, "writes": writes, "mem_accesses": mem_accesses}

if __name__ == "__main__":
    expr = input("Expr (ej: result = a - (b - c) + d): ")
    code, stats = compile_to_asua_with_stats(expr)
    print("\nCODE:")
    for line in code: print(line)
    print(f"\n# Métricas: {stats['lines']} líneas, {stats['reads']} lecturas, {stats['writes']} escrituras, {stats['mem_accesses']} accesos_totales")

