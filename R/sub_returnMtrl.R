
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
#' sub_returnMtrlSelectServer()
sub_returnMtrlSelectServer <- function(input,output,session,dms_token,erp_token) {
  #获取参数
  text_sub_returnMtrl_FBillNo=tsui::var_text('text_sub_returnMtrl_FBillNo')

  date_sub_returnMtrl_FDate=tsui::var_date('date_sub_returnMtrl_FDate')

  #查询按钮

  shiny::observeEvent(input$btn_sub_returnMtrl_view,{
    FSrcBillNo =text_sub_returnMtrl_FBillNo()
    FDate = date_sub_returnMtrl_FDate()

    data = mdlTMBillSyncPkg::sub_returnMtrl_log_view(dms_token = dms_token,FSrcBillNo =FSrcBillNo ,FDate = FDate)
    tsui::run_dataTable2(id ='dt_sub_returnMtrl' ,data =data )

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
#' sub_returnMtrlupdateServer()
sub_returnMtrlupdateServer <- function(input,output,session,dms_token,erp_token) {
  #获取参数
  text_sub_returnMtrl_FBillNo=tsui::var_text('text_sub_returnMtrl_FBillNo')

  date_sub_returnMtrl_FDate=tsui::var_date('date_sub_returnMtrl_FDate')

  #查询按钮

  shiny::observeEvent(input$btn_sub_returnMtrl_update,{
    FSrcBillNo =text_sub_returnMtrl_FBillNo()
    FDate = date_sub_returnMtrl_FDate()

    data = mdlTMBillSyncPkg::sub_returnMtrl_Fisdo_update(dms_token = dms_token,FSrcBillNo =FSrcBillNo ,FDate = FDate)
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
#' sub_returnMtrlSyncServer()
sub_returnMtrlSyncServer <- function(input,output,session,dms_token,erp_token,apsToken) {
  #获取参数
  text_sub_returnMtrl_FBillNo=tsui::var_text('text_sub_returnMtrl_FBillNo')

  date_sub_returnMtrl_FDate=tsui::var_date('date_sub_returnMtrl_FDate')

  #查询按钮

  shiny::observeEvent(input$btn_sub_returnMtrl_sync,{
    FSrcBillNo =text_sub_returnMtrl_FBillNo()
    FDate = date_sub_returnMtrl_FDate()

    result <- callr::r(function(token_api_erpKdc, FTokenDms, FSrcBillNo, FDate) {
      mdl <- tsda::import('pyapikdc.sub.ext.tm.subReturnMtrl')
      app <- mdl$SubReturnMtrlBill_TM(token = token_api_erpKdc)
      app$SyncOneManually(
        FTokenDms = FTokenDms,
        FSrcBillNo = FSrcBillNo,
        FDate = FDate
      )
    },
    args = list(
      token_api_erpKdc = apsToken,
      FTokenDms = dms_token,
      FSrcBillNo = FSrcBillNo,  # 现在可以作为参数修改
      FDate = FDate      # 现在可以作为参数修改
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
#' sub_returnMtrlServer()
sub_returnMtrlServer <- function(input,output,session,dms_token,erp_token,apsToken) {
  sub_returnMtrlSelectServer(input = input,output = output,session = session,dms_token = dms_token,erp_token=erp_token)


  sub_returnMtrlupdateServer(input = input,output = output,session = session,dms_token = dms_token,erp_token=erp_token)

  sub_returnMtrlSyncServer(input = input,output = output,session = session,dms_token = dms_token,erp_token=erp_token,apsToken=apsToken)

}

















