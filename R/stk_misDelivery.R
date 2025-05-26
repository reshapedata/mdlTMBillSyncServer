
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
#' stk_misDeliverySelectServer()
stk_misDeliverySelectServer <- function(input,output,session,dms_token,erp_token) {
  #获取参数
  text_stk_misDelivery_FBillTypeID=tsui::var_text('text_stk_misDelivery_FBillTypeID')

  text_stk_misDelivery_FDEPTID=tsui::var_text('text_stk_misDelivery_FDEPTID')

  text_stk_misDelivery_FPickerId=tsui::var_text('text_stk_misDelivery_FPickerId')

  date_stk_misDelivery_FDate=tsui::var_date('date_stk_misDelivery_FDate')

  #查询按钮

  shiny::observeEvent(input$btn_stk_misDelivery_view,{
    FBillTypeID =text_stk_misDelivery_FBillTypeID()
    FDEPTID =text_stk_misDelivery_FDEPTID()
    FPickerId=text_stk_misDelivery_FPickerId()
    FDate = date_stk_misDelivery_FDate()


    data = mdlTMBillSyncPkg::stk_misDelivery_log_view(dms_token = dms_token,FBillTypeID =FBillTypeID ,FDEPTID = FDEPTID,FPickerId = FPickerId,FDate = FDate)
    tsui::run_dataTable2(id ='dt_stk_misDelivery' ,data =data )

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
#' stk_misDeliveryupdateServer()
stk_misDeliveryupdateServer <- function(input,output,session,dms_token,erp_token) {
  #获取参数
  text_stk_misDelivery_FBillTypeID=tsui::var_text('text_stk_misDelivery_FBillTypeID')

  text_stk_misDelivery_FDEPTID=tsui::var_text('text_stk_misDelivery_FDEPTID')

  text_stk_misDelivery_FPickerId=tsui::var_text('text_stk_misDelivery_FPickerId')

  date_stk_misDelivery_FDate=tsui::var_date('date_stk_misDelivery_FDate')

  #查询按钮

  shiny::observeEvent(input$btn_stk_misDelivery_update,{
    FBillTypeID =text_stk_misDelivery_FBillTypeID()
    FDEPTID =text_stk_misDelivery_FDEPTID()
    FPickerId=text_stk_misDelivery_FPickerId()
    FDate = date_stk_misDelivery_FDate()
    data = mdlTMBillSyncPkg::stk_misDelivery_Fisdo_update(dms_token = dms_token,FBillTypeID =FBillTypeID ,FDEPTID = FDEPTID,FPickerId = FPickerId,FDate = FDate)
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
#' stk_misDeliverySyncServer()
stk_misDeliverySyncServer <- function(input,output,session,dms_token,erp_token,apsToken) {
  #获取参数
  text_stk_misDelivery_FBillTypeID=tsui::var_text('text_stk_misDelivery_FBillTypeID')

  text_stk_misDelivery_FDEPTID=tsui::var_text('text_stk_misDelivery_FDEPTID')

  text_stk_misDelivery_FPickerId=tsui::var_text('text_stk_misDelivery_FPickerId')

  date_stk_misDelivery_FDate=tsui::var_date('date_stk_misDelivery_FDate')

  #查询按钮

  shiny::observeEvent(input$btn_stk_misDelivery_sync,{
    FBillTypeID =text_stk_misDelivery_FBillTypeID()
    FDEPTID =text_stk_misDelivery_FDEPTID()
    FPickerId=text_stk_misDelivery_FPickerId()
    FDate = date_stk_misDelivery_FDate()

    result <- callr::r(function(token_api_erpKdc,FTokenDms, FDate, FBillTypeID,FDEPTID,FPickerId) {
      mdl <- tsda::import('pyapikdc.stk.ext.tm.misDelivery')
      app <- mdl$MisDeliveryBill_TM(token = token_api_erpKdc)
      app$SyncOneManually(
        FTokenDms = FTokenDms,
        FDate=FDate,
        FBillTypeID=FBillTypeID,
        FDEPTID=FDEPTID,
        FPickerId=FPickerId,
        debug=0
      )
    },
    args = list(
      token_api_erpKdc = apsToken,
      FTokenDms = dms_token,
      FDate=FDate,
      FBillTypeID=FBillTypeID,
      FDEPTID=FDEPTID,
      FPickerId=FPickerId
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
#' stk_misDeliveryServer()
stk_misDeliveryServer <- function(input,output,session,dms_token,erp_token,apsToken) {
  stk_misDeliverySelectServer(input = input,output = output,session = session,dms_token = dms_token,erp_token=erp_token)


  stk_misDeliveryupdateServer(input = input,output = output,session = session,dms_token = dms_token,erp_token=erp_token)

  stk_misDeliverySyncServer(input = input,output = output,session = session,dms_token = dms_token,erp_token=erp_token,apsToken=apsToken)

}













