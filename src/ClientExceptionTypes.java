public enum ClientExceptionTypes {
    INVALID_SYNTAX(0, "Invalid Syntax"),
    UNKNOWN_COMMAND(1, "Unknown Command"),
    INVALID_JOIN(2, "Invalid Join Syntax");

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