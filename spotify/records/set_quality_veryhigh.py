# noinspection PyUnusedLocal
import time
def main(device, *args, **kwargs):
    device.shell('input tap 1016 241')
    time.sleep(2)
    for i in range(3):
        device.shell('input swipe 540 2000 540 200 1000')
        time.sleep(1.5)
    device.shell('input tap 887 500')
    time.sleep(1.5)
    device.shell('input tap 887 1145') # 758 887 1016 1145 
    time.sleep(1.5)
    device.shell('input tap 56 161')
    time.sleep(1.5)