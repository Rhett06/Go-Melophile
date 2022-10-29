# noinspection PyUnusedLocal
import time
def main(device, *args, **kwargs):
    print('=INTERACTION=')
    print((device.id))
    print((device.current_activity()))
    time.sleep(180)
