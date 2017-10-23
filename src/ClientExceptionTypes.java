public enum ClientExceptionTypes {
    INVALID_SYNTAX(0, "Invalid syntax"),
    UNKNOWN_COMMAND(1, "Unknown command"),
    INVALID_JOIN(2, "Invalid join syntax"),
    NO_CHATROOM_FOUND(3, "No chat room found");

    private int errorCode;
    private String errorMessage;


    ClientExceptionTypes(int errorCode, String errorMessage) {
        this.errorCode = errorCode;
        this.errorMessage = errorMessage;
    }

    public String getErrorMessage(){
        return errorMessage;
    }

    public int getErrorCode() {
        return errorCode;
    }

}