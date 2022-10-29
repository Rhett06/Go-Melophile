# noinspection PyUnusedLocal
import time
def main(device, *args, **kwargs):
    device.shell('cmd media_session volume --show --stream 3 --set 8')
    time.sleep(5)
