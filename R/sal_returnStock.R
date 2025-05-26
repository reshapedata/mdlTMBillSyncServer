
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
#' sal_returnStockSelectServer()
sal_returnStockSelectServer <- function(input,output,session,dms_token,erp_token) {
  #获取参数
  text_sal_returnStock_FBillNo=tsui::var_text('text_sal_returnStock_FBillNo')

  date_sal_returnStock_FDate=tsui::var_date('date_sal_returnStock_FDate')

  #查询按钮

  shiny::observeEvent(input$btn_sal_returnStock_view,{
    FSrcBillNo =text_sal_returnStock_FBillNo()
    FDate = date_sal_returnStock_FDate()

    data = mdlTMBillSyncPkg::sal_returnStock_log_view(dms_token = dms_token,FSrcBillNo =FSrcBillNo ,FDate = FDate)
    tsui::run_dataTable2(id ='dt_sal_returnStock' ,data =data )

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
#' sal_returnStockupdateServer()
sal_returnStockupdateServer <- function(input,output,session,dms_token,erp_token) {
  #获取参数
  text_sal_returnStock_FBillNo=tsui::var_text('text_sal_returnStock_FBillNo')

  date_sal_returnStock_FDate=tsui::var_date('date_sal_returnStock_FDate')

  #查询按钮

  shiny::observeEvent(input$btn_sal_returnStock_update,{
    FSrcBillNo =text_sal_returnStock_FBillNo()
    FDate = date_sal_returnStock_FDate()

    data = mdlTMBillSyncPkg::sal_returnStock_Fisdo_update(dms_token = dms_token,FSrcBillNo =FSrcBillNo ,FDate = FDate)
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
#' sal_returnStockSyncServer()
sal_returnStockSyncServer <- function(input,output,session,dms_token,erp_token,apsToken) {
  #获取参数
  text_sal_returnStock_FBillNo=tsui::var_text('text_sal_returnStock_FBillNo')

  date_sal_returnStock_FDate=tsui::var_date('date_sal_returnStock_FDate')

  #查询按钮

  shiny::observeEvent(input$btn_sal_returnStock_sync,{
    FSrcBillNo =text_sal_returnStock_FBillNo()
    FDate = date_sal_returnStock_FDate()

    result <- callr::r(function(token_api_erpKdc, FTokenDms, FSrcBillNo, FDate) {
      mdl <- tsda::import('pyapikdc.sal.ext.tm.saleReturnStock')
      app <- mdl$SaleReturnStockBill_TM(token = token_api_erpKdc)
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
#' sal_returnStockServer()
sal_returnStockServer <- function(input,output,session,dms_token,erp_token,apsToken) {
  sal_returnStockSelectServer(input = input,output = output,session = session,dms_token = dms_token,erp_token=erp_token)


  sal_returnStockupdateServer(input = input,output = output,session = session,dms_token = dms_token,erp_token=erp_token)

  sal_returnStockSyncServer(input = input,output = output,session = session,dms_token = dms_token,erp_token=erp_token,apsToken=apsToken)

}















