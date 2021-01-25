
package com.reremouse.weka.util;

/**
 *
 * @author RereMouse
 */
public class TimeUtil {
    
    
    
    /**
     * 此方法用于将以秒为单位的时长变换为文本格式，用于在前端页面显示时间长度
     *
     * @param len 时间长度，以豪秒为单位
     * @return 返回 XX小时XX分XX秒 格式的文本
     */
    public static String wrapperTimeLen(long len) {
        len = Math.round(len / 1000d);
        String str = "";
        int hour = 0;
        int min = 0;
        int sec = 0;
        hour = (int) len / 3600;
        min = (int) (len % 3600) / 60;
        sec = (int) len % 60;
        if (hour != 0) {
            str += hour + "小时";
        }
        if (min != 0) {
            str += min + "分";
        }
        str += sec + "秒";
        return str;
    }
}
