import wir

while True:
    text = input('wir > ')
    result, err = wir.run_program('<a>', text)

    if err: print(err.string_form())
    elif result: print(result)