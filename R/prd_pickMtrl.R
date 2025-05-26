
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
#' prd_pickMtrlSelectServer()
prd_pickMtrlSelectServer <- function(input,output,session,dms_token,erp_token) {
  #获取参数
  text_prd_pickMtrl_FBillNo=tsui::var_text('text_prd_pickMtrl_FBillNo')

  date_prd_pickMtrl_FDate=tsui::var_date('date_prd_pickMtrl_FDate')

  #查询按钮

  shiny::observeEvent(input$btn_prd_pickMtrl_view,{
    FSrcBillNo =text_prd_pickMtrl_FBillNo()
    FDate = date_prd_pickMtrl_FDate()

    data = mdlTMBillSyncPkg::prd_pickMtrl_log_view(dms_token = dms_token,FSrcBillNo =FSrcBillNo ,FDate = FDate)
    tsui::run_dataTable2(id ='dt_prd_pickMtrl' ,data =data )

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
#' prd_pickMtrlupdateServer()
prd_pickMtrlupdateServer <- function(input,output,session,dms_token,erp_token) {
  #获取参数
  text_prd_pickMtrl_FBillNo=tsui::var_text('text_prd_pickMtrl_FBillNo')

  date_prd_pickMtrl_FDate=tsui::var_date('date_prd_pickMtrl_FDate')

  #查询按钮

  shiny::observeEvent(input$btn_prd_pickMtrl_update,{
    FSrcBillNo =text_prd_pickMtrl_FBillNo()
    FDate = date_prd_pickMtrl_FDate()

    data = mdlTMBillSyncPkg::prd_pickMtrl_Fisdo_update(dms_token = dms_token,FSrcBillNo =FSrcBillNo ,FDate = FDate)
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
#' prd_pickMtrlSyncServer()
prd_pickMtrlSyncServer <- function(input,output,session,dms_token,erp_token,apsToken) {
  #获取参数
  text_prd_pickMtrl_FBillNo=tsui::var_text('text_prd_pickMtrl_FBillNo')

  date_prd_pickMtrl_FDate=tsui::var_date('date_prd_pickMtrl_FDate')

  #查询按钮

  shiny::observeEvent(input$btn_prd_pickMtrl_sync,{
    FSrcBillNo =text_prd_pickMtrl_FBillNo()
    FDate = date_prd_pickMtrl_FDate()

    result <- callr::r(function(token_api_erpKdc, FTokenDms, FSrcBillNo, FDate) {
      mdl <- tsda::import('pyapikdc.prd.ext.tm.pickMtrl')
      app <- mdl$PickMtrlBill_TM(token = token_api_erpKdc)
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
#' prd_pickMtrlServer()
prd_pickMtrlServer <- function(input,output,session,dms_token,erp_token,apsToken) {
  prd_pickMtrlSelectServer(input = input,output = output,session = session,dms_token = dms_token,erp_token=erp_token)


  prd_pickMtrlupdateServer(input = input,output = output,session = session,dms_token = dms_token,erp_token=erp_token)

  prd_pickMtrlSyncServer(input = input,output = output,session = session,dms_token = dms_token,erp_token=erp_token,apsToken=apsToken)

}

















