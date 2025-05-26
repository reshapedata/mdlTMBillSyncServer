
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
#' stk_AssembledAppSelectServer()
stk_AssembledAppSelectServer <- function(input,output,session,dms_token,erp_token) {
  #获取参数
  text_stk_AssembledApp_FBillNo=tsui::var_text('text_stk_AssembledApp_FBillNo')

  date_stk_AssembledApp_FDate=tsui::var_date('date_stk_AssembledApp_FDate')

  #查询按钮

  shiny::observeEvent(input$btn_stk_AssembledApp_view,{
    FSrcBillNo =text_stk_AssembledApp_FBillNo()
    FDate = date_stk_AssembledApp_FDate()

    data = mdlTMBillSyncPkg::stk_AssembledApp_log_view(dms_token = dms_token,FSrcBillNo =FSrcBillNo ,FDate = FDate)
    tsui::run_dataTable2(id ='dt_stk_AssembledApp' ,data =data )

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
#' stk_AssembledAppupdateServer()
stk_AssembledAppupdateServer <- function(input,output,session,dms_token,erp_token) {
  #获取参数
  text_stk_AssembledApp_FBillNo=tsui::var_text('text_stk_AssembledApp_FBillNo')

  date_stk_AssembledApp_FDate=tsui::var_date('date_stk_AssembledApp_FDate')

  #查询按钮

  shiny::observeEvent(input$btn_stk_AssembledApp_update,{
    FSrcBillNo =text_stk_AssembledApp_FBillNo()
    FDate = date_stk_AssembledApp_FDate()

    data = mdlTMBillSyncPkg::stk_AssembledApp_Fisdo_update(dms_token = dms_token,FSrcBillNo =FSrcBillNo ,FDate = FDate)
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
#' stk_AssembledAppSyncServer()
stk_AssembledAppSyncServer <- function(input,output,session,dms_token,erp_token,apsToken) {
  #获取参数
  text_stk_AssembledApp_FBillNo=tsui::var_text('text_stk_AssembledApp_FBillNo')

  date_stk_AssembledApp_FDate=tsui::var_date('date_stk_AssembledApp_FDate')

  #查询按钮

  shiny::observeEvent(input$btn_stk_AssembledApp_sync,{
    FSrcBillNo =text_stk_AssembledApp_FBillNo()
    FDate = date_stk_AssembledApp_FDate()

    result <- callr::r(function(token_api_erpKdc, FTokenDms, FDate, FSrcBillNo) {
      mdl <- tsda::import('pyapikdc.stk.ext.tm.assembledApp')
      app <- mdl$AssembledAppBill_TM(token = token_api_erpKdc)
      app$SyncOneManually(
        FTokenDms = FTokenDms,
        FDate = FDate,
        FSrcBillNo = FSrcBillNo

      )
    },
    args = list(
      token_api_erpKdc = apsToken,
      FTokenDms = dms_token,
      FDate = FDate    ,  # 现在可以作为参数修改
      FSrcBillNo = FSrcBillNo

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
#' stk_AssembledAppServer()
stk_AssembledAppServer <- function(input,output,session,dms_token,erp_token,apsToken) {
  stk_AssembledAppSelectServer(input = input,output = output,session = session,dms_token = dms_token,erp_token=erp_token)


  stk_AssembledAppupdateServer(input = input,output = output,session = session,dms_token = dms_token,erp_token=erp_token)

  stk_AssembledAppSyncServer(input = input,output = output,session = session,dms_token = dms_token,erp_token=erp_token,apsToken=apsToken)

}
















