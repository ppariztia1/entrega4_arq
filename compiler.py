# compiler_asua.py
# Compilador parcial ASUA: solo +, -, paréntesis
# Genera el bloque CODE que se pega en el ASUA Emulator

import re

def tokenize(expr):
    expr = expr.replace('(', ' ( ').replace(')', ' ) ')
    return expr.split()

def flatten(expr):
    tokens = tokenize(expr)
    sign_stack = [1]
    pending = 1
    result = []

    for t in tokens:
        if t == '+':
            pending = 1
        elif t == '-':
            pending = -1
        elif t == '(':
            sign_stack.append(sign_stack[-1] * pending)
            pending = 1
        elif t == ')':
            sign_stack.pop()
        else:
            result.append((sign_stack[-1] * pending, t))
            pending = 1

    return result

def compile_to_asua(expr):
    lhs, rhs = expr.split('=')
    lhs = lhs.strip()
    rhs = rhs.strip()
    terms = flatten(rhs)

    code = []
    # Encuentra primer término positivo (si hay)
    pos_idx = next((i for i, (s, _) in enumerate(terms) if s == 1), None)

    if pos_idx is None:
        # Todos negativos
        code.append("MOV A,0")
        for _, var in terms:
            code.append(f"MOV B,(v_{var})")
            code.append("SUB A,B")
    else:
        # Empieza con el primer positivo
        s, var = terms[pos_idx]
        code.append(f"MOV A,(v_{var})")
        for i, (sign, var) in enumerate(terms):
            if i == pos_idx:
                continue
            code.append(f"MOV B,(v_{var})")
            if sign == 1:
                code.append("ADD A,B")
            else:
                code.append("SUB A,B")

    code.append(f"MOV (result),A")
    return "\n".join(code)

if __name__ == "__main__":
    expr = input("Ingresa la expresión (ej: result = a - (b - c) + d): ")
    print("\nCODE:")
    print(compile_to_asua(expr))
