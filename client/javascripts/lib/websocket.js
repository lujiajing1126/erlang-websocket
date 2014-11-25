/**
 * whosvIO Web IM Client
 * @author megrez
 * @version 0.1.0
 **/
;
(function(window) {
    window.whosvIO = whosvIO = {};

    /**
     * Custom Error
     */
    whosvIO.BaseException = function() {};
    whosvIO.BaseException.prototype = new Error();
    whosvIO.BaseException.constructor = whosvIO.BaseException;
    whosvIO.BaseException.toString = function() {
        return this.name + ": " + this.message;
    };
    whosvIO.BrowserNotSupportError = function() {
        this.name = "BrowserNotSupportError";
        this.message = "Your Browser Do Not Support Native WebSocket Protocol";
    };
    whosvIO.BrowserNotSupportError.prototype = new whosvIO.BaseException();
    whosvIO.BrowserNotSupportError.constructor = whosvIO.BrowserNotSupportError;

    /**
     * Event Manager
     */
    var EventManager = function() {
        this.listeners = [];
    };
    EventManager.prototype.fireEvent = function(eventName, eventProperties) {
        if (!this.listeners[eventName])
            return;
        for (var i = 0; i < this.listeners[eventName].length; i++) {
            this.listeners[eventName][i](eventProperties);
        }
    };
    EventManager.prototype.addListener = function(eventName, callback) {
        if (!this.listeners[eventName])
            this.listeners[eventName] = [];
        this.listeners[eventName].push(callback);
    };
    EventManager.prototype.removeListener = function(eventName, callback) {
        if (!this.listeners[eventName])
            return;
        for (var i = 0; i < this.listeners[eventName].length; i++) {
            if (this.listeners[eventName][i] == callback) {
                delete this.listeners[eventName][i];
                return;
            }
        }
    };
    /**
     * Socket
     * @param  {String} ws WebSocket链接
     * @return {socket} socket对象
     */
    var socket = function(ws) {
        var _this = this;
        this.websocket = this.connect(ws);
        this.EventManager = new EventManager();
        this.websocket.onopen = function() {
            _this.EventManager.fireEvent("open");
        };
        this.websocket.onmessage = function(evt) {
            _this.EventManager.fireEvent("message",evt);
        };
        this.websocket.onclose = function() {
            _this.EventManager.fireEvent("close");
        };
    };
    socket.prototype.connect = function(ws) {
        return new WebSocket(ws);
    }
    socket.prototype.on = function(event,callback) {
        this.EventManager.addListener(event,callback);
    }
    socket.prototype.emit = function(message, to) {
        this.send({
            data: {
                message: message
            },
            to: to,
            type: "message"
        });
    }
    socket.prototype.login = function(username, password) {
        this.send({
            data: {
                username: username,
                password: password
            },
            type: "system"
        });
    }
    socket.prototype.send = function(data) {
        this.websocket.send(JSON.stringify(data));
    }

    /**
     * Interface
     * @param  {Function} callback [description]
     */
    whosvIO.ready = function(callback) {
        if ("WebSocket" in window) {
            callback(socket);
        } else {
            throw new whosvIO.browserNotSupportError();
        }
    };
})(window);