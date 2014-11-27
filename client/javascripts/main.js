$(document).ready(function() {
    try {
        whosvIO.ready(function(socket){
            var ws = new socket("ws://localhost:12345");
            function handleFile(file,to) {
                console.log("handleFile");
                var reader = new FileReader(),
                    img = $('<img>');
                reader.onload = (function(aImg) {
                    return function(e) {
                        img.attr('src',e.target.result);
                        $("#content").append(aImg);
                        console.log(e.target.result);
                        ws.emit(e.target.result,'img',to);
                    };
                })(img);
                reader.readAsDataURL(file);
            }
            ws.on("open",function() {
                $("#content").append("<p style='color: #80ff00;'>websocket connected!</p>");
                $("#loginbox").show();
            });
            ws.on("close",function(){
                $("#content").append("<p style='color: #ff3737;'>websocket closed!</p>");
            });
            ws.on("message",function(evt){
                var data = evt.data;
                $("#content").append("<p> <暂时未实现> 对你说: " +  data + "</p>");
            });
            $("#smt").click(function() {
                var val = $.trim($("#msg").val()),
                    to_user = $.trim($("#to").val());
                if ( val || to_user)
                {
                    ws.emit(val,'txt',to_user);
                } else {
                    alert("消息不能为空");
                }
                $("#msg").val("");
                $("#content").append("<p>你对 "+ to_user + "说: " +  val + "</p>");
                return false;
            });
            $("#imageUploader").change(function(){
                console.log(this.files);
                handleFile(this.files[0],$.trim($("#to").val()));
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