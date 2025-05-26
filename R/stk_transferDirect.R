
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
#' stk_transferDirectSelectServer()
stk_transferDirectSelectServer <- function(input,output,session,dms_token,erp_token) {
  #获取参数
  text_stk_transferDirect_FBillTypeID=tsui::var_text('text_stk_transferDirect_FBillTypeID')

  text_stk_transferDirect_FDEPTID=tsui::var_text('text_stk_transferDirect_FDEPTID')

  date_stk_transferDirect_FDate=tsui::var_date('date_stk_transferDirect_FDate')

  #查询按钮

  shiny::observeEvent(input$btn_stk_transferDirect_view,{
    FBillTypeID =text_stk_transferDirect_FBillTypeID()
    FINDEPTID =text_stk_transferDirect_FDEPTID()
    FDate = date_stk_transferDirect_FDate()


    data = mdlTMBillSyncPkg::stk_transferDirect_log_view(dms_token = dms_token,FBillTypeID = FBillTypeID,FINDEPTID =FINDEPTID ,FDate = FDate)
    tsui::run_dataTable2(id ='dt_stk_transferDirect' ,data =data )

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
#' stk_transferDirectupdateServer()
stk_transferDirectupdateServer <- function(input,output,session,dms_token,erp_token) {
  #获取参数
  text_stk_transferDirect_FBillTypeID=tsui::var_text('text_stk_transferDirect_FBillTypeID')

  text_stk_transferDirect_FDEPTID=tsui::var_text('text_stk_transferDirect_FDEPTID')

  date_stk_transferDirect_FDate=tsui::var_date('date_stk_transferDirect_FDate')

  #查询按钮

  shiny::observeEvent(input$btn_stk_transferDirect_update,{
    FBillTypeID =text_stk_transferDirect_FBillTypeID()
    FINDEPTID =text_stk_transferDirect_FDEPTID()
    FDate = date_stk_transferDirect_FDate()

    data = mdlTMBillSyncPkg::stk_transferDirect_Fisdo_update(dms_token = dms_token,FBillTypeID = FBillTypeID,FINDEPTID =FINDEPTID ,FDate = FDate)
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
#' stk_transferDirectSyncServer()
stk_transferDirectSyncServer <- function(input,output,session,dms_token,erp_token,apsToken) {
  #获取参数
  text_stk_transferDirect_FBillTypeID=tsui::var_text('text_stk_transferDirect_FBillTypeID')

  text_stk_transferDirect_FDEPTID=tsui::var_text('text_stk_transferDirect_FDEPTID')

  date_stk_transferDirect_FDate=tsui::var_date('date_stk_transferDirect_FDate')

  #查询按钮

  shiny::observeEvent(input$btn_stk_transferDirect_sync,{
    FBillTypeID =text_stk_transferDirect_FBillTypeID()
    FDEPTID =text_stk_transferDirect_FDEPTID()
    FDate = date_stk_transferDirect_FDate()

    result <- callr::r(function(token_api_erpKdc,FTokenDms,FDate,FBillTypeID,FDEPTID) {
      mdl <- tsda::import('pyapikdc.stk.ext.tm.transferDirect')
      app <- mdl$TransferDirectBill_TM(token = token_api_erpKdc)
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
#' stk_transferDirectServer()
stk_transferDirectServer <- function(input,output,session,dms_token,erp_token,apsToken) {
  stk_transferDirectSelectServer(input = input,output = output,session = session,dms_token = dms_token,erp_token=erp_token)


  stk_transferDirectupdateServer(input = input,output = output,session = session,dms_token = dms_token,erp_token=erp_token)

  stk_transferDirectSyncServer(input = input,output = output,session = session,dms_token = dms_token,erp_token=erp_token,apsToken=apsToken)

}















