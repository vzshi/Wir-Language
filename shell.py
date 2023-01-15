import wir

while True:
    text = input('wir > ')
    if text.strip() == "": continue
    result, err = wir.run_program('<stdin>', text)

    if err: print(err.string_form())
    elif result:
        if len(result.elements) == 1:
            print(repr(result.elements[0]))
        else:
            print(repr(result))