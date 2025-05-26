
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
#' pur_inStockSelectServer()
pur_inStockSelectServer <- function(input,output,session,dms_token,erp_token) {
  #获取参数
  text_pur_inStock_FBillNo=tsui::var_text('text_pur_inStock_FBillNo')

  date_pur_inStock_FDate=tsui::var_date('date_pur_inStock_FDate')

  #查询按钮

  shiny::observeEvent(input$btn_pur_inStock_view,{
    FSrcBillNo =text_pur_inStock_FBillNo()
    FDate = date_pur_inStock_FDate()

    data = mdlTMBillSyncPkg::pur_inStock_log_view(dms_token = dms_token,FSrcBillNo =FSrcBillNo ,FDate = FDate)
    tsui::run_dataTable2(id ='dt_pur_inStock' ,data =data )

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
#' pur_inStockupdateServer()
pur_inStockupdateServer <- function(input,output,session,dms_token,erp_token) {
  #获取参数
  text_pur_inStock_FBillNo=tsui::var_text('text_pur_inStock_FBillNo')

  date_pur_inStock_FDate=tsui::var_date('date_pur_inStock_FDate')

  #查询按钮

  shiny::observeEvent(input$btn_pur_inStock_update,{
    FSrcBillNo =text_pur_inStock_FBillNo()
    FDate = date_pur_inStock_FDate()

    data = mdlTMBillSyncPkg::pur_inStock_Fisdo_update(dms_token = dms_token,FSrcBillNo =FSrcBillNo ,FDate = FDate)
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
#' pur_inStockSyncServer()
pur_inStockSyncServer <- function(input,output,session,dms_token,erp_token,apsToken) {
  #获取参数
  text_pur_inStock_FBillNo=tsui::var_text('text_pur_inStock_FBillNo')

  date_pur_inStock_FDate=tsui::var_date('date_pur_inStock_FDate')

  #查询按钮

  shiny::observeEvent(input$btn_pur_inStock_sync,{
    FSrcBillNo =text_pur_inStock_FBillNo()
    FDate = date_pur_inStock_FDate()

    result <- callr::r(function(token_api_erpKdc, FTokenDms, FSrcBillNo, FDate) {
      mdl <- tsda::import('pyapikdc.pur.ext.tm.purInStock')
      app <- mdl$PurInStockBill_TM(token = token_api_erpKdc)
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
#' pur_inStockServer()
pur_inStockServer <- function(input,output,session,dms_token,erp_token,apsToken) {
  pur_inStockSelectServer(input = input,output = output,session = session,dms_token = dms_token,erp_token=erp_token)


  pur_inStockupdateServer(input = input,output = output,session = session,dms_token = dms_token,erp_token=erp_token)

  pur_inStockSyncServer(input = input,output = output,session = session,dms_token = dms_token,erp_token=erp_token,apsToken=apsToken)


}















