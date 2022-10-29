# noinspection PyUnusedLocal
import time
def main(device, *args, **kwargs):
    time.sleep(10)
    device.shell('input tap 540 1854')
    time.sleep(5)
    device.shell('input tap 540 1370')
    time.sleep(10)
