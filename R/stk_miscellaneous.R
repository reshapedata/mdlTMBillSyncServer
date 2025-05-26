
#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' stk_miscellaneousSelectServer()
stk_miscellaneousSelectServer <- function(input,output,session,dms_token,erp_token) {
  #获取参数
  text_stk_miscellaneous_FBillTypeID=tsui::var_text('text_stk_miscellaneous_FBillTypeID')

  text_stk_miscellaneous_FDEPTID=tsui::var_text('text_stk_miscellaneous_FDEPTID')

  date_stk_miscellaneous_FDate=tsui::var_date('date_stk_miscellaneous_FDate')

  #查询按钮

  shiny::observeEvent(input$btn_stk_miscellaneous_view,{
    FBillTypeID =text_stk_miscellaneous_FBillTypeID()
    FDEPTID =text_stk_miscellaneous_FDEPTID()
    FDate = date_stk_miscellaneous_FDate()


    data = mdlTMBillSyncPkg::stk_miscellaneous_log_view(dms_token = dms_token,FBillTypeID = FBillTypeID,FDEPTID = FDEPTID,FDate = FDate)
    tsui::run_dataTable2(id ='dt_stk_miscellaneous' ,data =data )

  })



}

#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' stk_miscellaneousupdateServer()
stk_miscellaneousupdateServer <- function(input,output,session,dms_token,erp_token) {
  #获取参数
  text_stk_miscellaneous_FBillTypeID=tsui::var_text('text_stk_miscellaneous_FBillTypeID')

  text_stk_miscellaneous_FDEPTID=tsui::var_text('text_stk_miscellaneous_FDEPTID')

  date_stk_miscellaneous_FDate=tsui::var_date('date_stk_miscellaneous_FDate')

  #查询按钮

  shiny::observeEvent(input$btn_stk_miscellaneous_update,{
    FBillTypeID =text_stk_miscellaneous_FBillTypeID()
    FDEPTID =text_stk_miscellaneous_FDEPTID()
    FDate = date_stk_miscellaneous_FDate()

    data = mdlTMBillSyncPkg::stk_miscellaneous_Fisdo_update(dms_token = dms_token,FBillTypeID = FBillTypeID,FDEPTID = FDEPTID,FDate = FDate)
    tsui::pop_notice('更新成功')

  })



}

#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#' @param erp_token 口令
#' @param apsToken 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' stk_miscellaneousSyncServer()
stk_miscellaneousSyncServer <- function(input,output,session,dms_token,erp_token,apsToken) {
  #获取参数
  text_stk_miscellaneous_FBillTypeID=tsui::var_text('text_stk_miscellaneous_FBillTypeID')

  text_stk_miscellaneous_FDEPTID=tsui::var_text('text_stk_miscellaneous_FDEPTID')

  date_stk_miscellaneous_FDate=tsui::var_date('date_stk_miscellaneous_FDate')

  #查询按钮

  shiny::observeEvent(input$btn_stk_miscellaneous_sync,{
    FBillTypeID =text_stk_miscellaneous_FBillTypeID()
    FDEPTID =text_stk_miscellaneous_FDEPTID()
    FDate = date_stk_miscellaneous_FDate()

    result <- callr::r(function(token_api_erpKdc,FTokenDms,FDate, FBillTypeID,FDEPTID) {
      mdl <- tsda::import('pyapikdc.stk.ext.tm.misCellaneous')
      app <- mdl$MisCellaneousBill_TM(token = token_api_erpKdc)
      app$SyncOneManually(
        FTokenDms = FTokenDms,
        FDate=FDate,
        FBillTypeID=FBillTypeID,
        FDEPTID=FDEPTID,
        debug=0
      )
    },
    args = list(
      token_api_erpKdc = apsToken,
      FTokenDms = dms_token,
      FDate=FDate,
      FBillTypeID=FBillTypeID,
      FDEPTID=FDEPTID
    ))

    tsui::pop_notice(result)

  })



}


#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' stk_miscellaneousServer()
stk_miscellaneousServer <- function(input,output,session,dms_token,erp_token,apsToken) {
  stk_miscellaneousSelectServer(input = input,output = output,session = session,dms_token = dms_token,erp_token=erp_token)


  stk_miscellaneousupdateServer(input = input,output = output,session = session,dms_token = dms_token,erp_token=erp_token)

  stk_miscellaneousSyncServer(input = input,output = output,session = session,dms_token = dms_token,erp_token=erp_token,apsToken=apsToken)

}














