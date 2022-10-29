# noinspection PyUnusedLocal
import time
def main(device, *args, **kwargs):
    device.shell('svc wifi disable')
    time.sleep(3)
