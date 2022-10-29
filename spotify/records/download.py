# noinspection PyUnusedLocal
import time
def main(device, *args, **kwargs):
    device.shell('input tap 822 2209')
    time.sleep(2)
    device.shell('input tap 129 1096')
    time.sleep(2)
    device.shell('input tap 346 1129')
    time.sleep(60)