# noinspection PyUnusedLocal
import time
def main(device, *args, **kwargs):
    device.shell('svc wifi enable')
    # device.shell('pm trim-caches 128G')
    device.shell('pm clear com.google.android.apps.youtube.music')
    time.sleep(20)
