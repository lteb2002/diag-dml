package com.reremouse.ju4ja.client;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.reremouse.ju4ja.msg.JavaCallMsg;
import com.reremouse.ju4ja.msg.JavaCallResult;
import com.reremouse.ju4ja.msg.OperationType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.*;
import java.net.Socket;

public class JuliaWorker implements Serializable {

    private Socket socket;
    private Logger logger = LoggerFactory.getLogger(Ju4jaClient.class);
    String tempJsonPath = "d:\\temp_json_for_ju4ja\\";


    /**
     * @param ip
     * @param port
     */
    public JuliaWorker(String ip, int port) {
        try {
            //创建客户端Socket，指定服务器地址和端口
            socket = new Socket(ip, port);
            socket.setSoTimeout(1000 * 3600 * 24);
            socket.setSendBufferSize(Integer.MAX_VALUE);
            socket.setReceiveBufferSize(Integer.MAX_VALUE);
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }

    /**
     * Send JSON message to Julia server
     *
     * @param msg Json messages
     * @return
     */
    private JavaCallResult sendCommand(String msg) {
        JavaCallResult result = null;
        try {
            //输出流，向服务器端发送信息
            OutputStream os = new BufferedOutputStream(socket.getOutputStream());
            PrintWriter pw = new PrintWriter(os);
            pw.println(msg);
            pw.flush();
            System.out.println("数据流已发出，等待对方响应。。。");
            //输入流，并读取服务器端的响应信息
            InputStream is = socket.getInputStream();
            BufferedReader br = new BufferedReader(new InputStreamReader(is));
            StringBuilder rs = new StringBuilder();
            String info = null;
            while ((info = br.readLine()) != null) {
                rs.append(info);
                logger.debug(info);
            }
            String json = rs.toString();
            ObjectMapper mapper = new ObjectMapper();
            result = mapper.readValue(json, JavaCallResult.class);
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        return result;
    }

    /**
     * @param func       function to be called in Julia
     * @param moduleName module name of function in Julia
     * @param args       arguments passed to Julia function
     * @return
     */
    public <T> JavaCallResult invokeFunction(String func, String moduleName, Object[] args) {
        JavaCallResult re = null;
        try {
            JavaCallMsg ins = new JavaCallMsg();
            ins.setFunc(func);
            ins.setModn(moduleName);
            //ins.setResultType(T);
            ins.setOperation(OperationType.FUNCTION);
            ins.setArgs(args);
            File pp = new File(this.tempJsonPath);
            if (!pp.exists()) {
                pp.mkdirs();
            }
            long id = System.nanoTime();
            String filePath = this.tempJsonPath + id + ".json";
            ins.setTempFilePath(filePath);
            ObjectMapper mapper = new ObjectMapper();
            String json = mapper.writeValueAsString(ins);
            //内容写入临时文件
            try {
                pp = new File(filePath);
                OutputStream os = new BufferedOutputStream(new FileOutputStream(pp));
                PrintWriter pw = new PrintWriter(os);
                pw.println(json);
                pw.flush();
                pw.close();
            } catch (Exception e) {
                e.printStackTrace();
            }
            ins.setArgs(null);//如果写了文件，就将args设为空，节省socket空间
            json = mapper.writeValueAsString(ins);
            logger.debug(json);
            System.out.println("JSON消息已生成！");
            re = this.sendCommand(json);
            if (re == null) {
                logger.error("Fatal inner error caused by remote Julia server or network.");
            }
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        return re;
    }

    /**
     *
     */
    public void close() {
        //关闭资源
        try {
            socket.close();
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }


}
