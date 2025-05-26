
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
#' sub_pickMtrlSelectServer()
sub_pickMtrlSelectServer <- function(input,output,session,dms_token,erp_token) {
  #获取参数
  text_sub_pickMtrl_FBillNo=tsui::var_text('text_sub_pickMtrl_FBillNo')

  date_sub_pickMtrl_FDate=tsui::var_date('date_sub_pickMtrl_FDate')

  #查询按钮

  shiny::observeEvent(input$btn_sub_pickMtrl_view,{
    FSrcBillNo =text_sub_pickMtrl_FBillNo()
    FDate = date_sub_pickMtrl_FDate()

    data = mdlTMBillSyncPkg::sub_pickMtrl_log_view(dms_token = dms_token,FSrcBillNo =FSrcBillNo ,FDate = FDate)
    tsui::run_dataTable2(id ='dt_sub_pickMtrl' ,data =data )

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
#' sub_pickMtrlupdateServer()
sub_pickMtrlupdateServer <- function(input,output,session,dms_token,erp_token) {
  #获取参数
  text_sub_pickMtrl_FBillNo=tsui::var_text('text_sub_pickMtrl_FBillNo')

  date_sub_pickMtrl_FDate=tsui::var_date('date_sub_pickMtrl_FDate')

  #查询按钮

  shiny::observeEvent(input$btn_sub_pickMtrl_update,{
    FSrcBillNo =text_sub_pickMtrl_FBillNo()
    FDate = date_sub_pickMtrl_FDate()

    data = mdlTMBillSyncPkg::sub_pickMtrl_Fisdo_update(dms_token = dms_token,FSrcBillNo =FSrcBillNo ,FDate = FDate)
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
#' sub_pickMtrlSyncServer()
sub_pickMtrlSyncServer <- function(input,output,session,dms_token,erp_token,apsToken) {
  #获取参数
  text_sub_pickMtrl_FBillNo=tsui::var_text('text_sub_pickMtrl_FBillNo')

  date_sub_pickMtrl_FDate=tsui::var_date('date_sub_pickMtrl_FDate')

  #查询按钮

  shiny::observeEvent(input$btn_sub_pickMtrl_sync,{
    FSrcBillNo =text_sub_pickMtrl_FBillNo()
    FDate = date_sub_pickMtrl_FDate()

    result <- callr::r(function(token_api_erpKdc, FTokenDms, FSrcBillNo, FDate) {
      mdl <- tsda::import('pyapikdc.sub.ext.tm.subPickMtrl')
      app <- mdl$SubPickMtrlBill_TM(token = token_api_erpKdc)
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
#' sub_pickMtrlServer()
sub_pickMtrlServer <- function(input,output,session,dms_token,erp_token,apsToken) {
  sub_pickMtrlSelectServer(input = input,output = output,session = session,dms_token = dms_token,erp_token=erp_token)


  sub_pickMtrlupdateServer(input = input,output = output,session = session,dms_token = dms_token,erp_token=erp_token)

  sub_pickMtrlSyncServer(input = input,output = output,session = session,dms_token = dms_token,erp_token=erp_token,apsToken=apsToken)

}














