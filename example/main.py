# main.py
from greeting import hello

print('>>> main.py')
    
if __name__ == '__main__':
    print('>>> This is the main program')
    hello()
else:
    print('>>> This is an imported program')
