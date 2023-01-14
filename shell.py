import wir

while True:
    text = input('wir > ')
    if text.strip() == "": continue
    result, err = wir.run_program('<a>', text)

    if err: print(err.string_form())
    elif result:
        if len(result.elements) == 1:
            print(repr(result.elements))
        else:
            print(repr(result))