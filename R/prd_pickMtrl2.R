
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
#' prd_pickMtrl2SelectServer()
prd_pickMtrl2SelectServer <- function(input,output,session,dms_token,erp_token) {
  #获取参数
  text_prd_pickMtrl2_FBillNo=tsui::var_text('text_prd_pickMtrl2_FBillNo')

  date_prd_pickMtrl2_FDate=tsui::var_date('date_prd_pickMtrl2_FDate')

  #查询按钮

  shiny::observeEvent(input$btn_prd_pickMtrl2_view,{
    FSrcBillNo =text_prd_pickMtrl2_FBillNo()
    FDate = date_prd_pickMtrl2_FDate()

    data = mdlTMBillSyncPkg::prd_pickMtrl2_log_view(dms_token = dms_token,FSrcBillNo =FSrcBillNo ,FDate = FDate)
    tsui::run_dataTable2(id ='dt_prd_pickMtrl2' ,data =data )

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
#' prd_pickMtrl2updateServer()
prd_pickMtrl2updateServer <- function(input,output,session,dms_token,erp_token) {
  #获取参数
  text_prd_pickMtrl2_FBillNo=tsui::var_text('text_prd_pickMtrl2_FBillNo')

  date_prd_pickMtrl2_FDate=tsui::var_date('date_prd_pickMtrl2_FDate')

  #查询按钮

  shiny::observeEvent(input$btn_prd_pickMtrl2_update,{
    FSrcBillNo =text_prd_pickMtrl2_FBillNo()
    FDate = date_prd_pickMtrl2_FDate()

    data = mdlTMBillSyncPkg::prd_pickMtrl2_Fisdo_update(dms_token = dms_token,FSrcBillNo =FSrcBillNo ,FDate = FDate)
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
#' prd_pickMtrl2SyncServer()
prd_pickMtrl2SyncServer <- function(input,output,session,dms_token,erp_token,apsToken) {
  #获取参数
  text_prd_pickMtrl2_FBillNo=tsui::var_text('text_prd_pickMtrl2_FBillNo')

  date_prd_pickMtrl2_FDate=tsui::var_date('date_prd_pickMtrl2_FDate')

  #查询按钮

  shiny::observeEvent(input$btn_prd_pickMtrl2_sync,{
    FSrcBillNo =text_prd_pickMtrl2_FBillNo()
    FDate = date_prd_pickMtrl2_FDate()

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
#' prd_pickMtrl2Server()
prd_pickMtrl2Server <- function(input,output,session,dms_token,erp_token,apsToken) {
  prd_pickMtrl2SelectServer(input = input,output = output,session = session,dms_token = dms_token,erp_token=erp_token)


  prd_pickMtrl2updateServer(input = input,output = output,session = session,dms_token = dms_token,erp_token=erp_token)

  prd_pickMtrl2SyncServer(input = input,output = output,session = session,dms_token = dms_token,erp_token=erp_token,apsToken=apsToken)

}
















