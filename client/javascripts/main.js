$(document).ready(function() {
    try {
        whosvIO.ready(function(socket){
            var ws = new socket("ws://192.168.2.123:12345");
            ws.on("open",function() {
                $("#content").append("<p style='color: #80ff00;'>websocket connected!</p>");
                $("#loginbox").show();
            });
            ws.on("close",function(){
                $("#content").append("<p style='color: #ff3737;'>websocket closed!</p>");
            });
            ws.on("message",function(evt){
                var data = evt.data;
                $("#content").append("<p>" +  data + "</p>");
            });
            $("#smt").click(function() {
                var val = $("#msg").val();
                if ( val )
                {
                    ws.emit(val,$("#to").val());
                }
                return false;
            });
            $("#login").click(function(){
                var username = $("#username").val(),
                    password = $("#password").val();
                ws.login(username,password);
                $("#loginbox").hide();
                $("#msgbox").show();
            });
        });
    } catch(e) {
        $("#content").append("<p style='color: #ff3737;'>"+e.toString()+"</p>");
    }
});