import wir

while True:
    text = input('wir > ')
    res, err = wir.run_program(text)

    if err: print(err.string_form())
    else: print(res)