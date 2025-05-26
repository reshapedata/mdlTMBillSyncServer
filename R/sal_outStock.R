
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
#' sal_outStockSelectServer()
sal_outStockSelectServer <- function(input,output,session,dms_token,erp_token) {
  #获取参数
  text_sal_outStock_FBillNo=tsui::var_text('text_sal_outStock_FBillNo')

  date_sal_outStock_FDate=tsui::var_date('date_sal_outStock_FDate')

  #查询按钮

  shiny::observeEvent(input$btn_sal_outStock_view,{
    FSrcBillNo =text_sal_outStock_FBillNo()
    FDate = date_sal_outStock_FDate()

    data = mdlTMBillSyncPkg::sal_outStock_log_view(dms_token = dms_token,FSrcBillNo =FSrcBillNo ,FDate = FDate)
    tsui::run_dataTable2(id ='dt_sal_outStock' ,data =data )

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
#' sal_outStockupdateServer()
sal_outStockupdateServer <- function(input,output,session,dms_token,erp_token) {
  #获取参数
  text_sal_outStock_FBillNo=tsui::var_text('text_sal_outStock_FBillNo')

  date_sal_outStock_FDate=tsui::var_date('date_sal_outStock_FDate')

  #查询按钮

  shiny::observeEvent(input$btn_sal_outStock_update,{
    FSrcBillNo =text_sal_outStock_FBillNo()
    FDate = date_sal_outStock_FDate()

    data = mdlTMBillSyncPkg::sal_outStock_Fisdo_update(dms_token = dms_token,FSrcBillNo =FSrcBillNo ,FDate = FDate)
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
#' sal_outStockSyncServer()
sal_outStockSyncServer <- function(input,output,session,dms_token,erp_token,apsToken) {
  #获取参数
  text_sal_outStock_FBillNo=tsui::var_text('text_sal_outStock_FBillNo')

  date_sal_outStock_FDate=tsui::var_date('date_sal_outStock_FDate')

  #查询按钮

  shiny::observeEvent(input$btn_sal_outStock_sync,{
    FSrcBillNo =text_sal_outStock_FBillNo()
    FDate = date_sal_outStock_FDate()

    result <- callr::r(function(token_api_erpKdc, FTokenDms, FSrcBillNo, FDate) {
      mdl <- tsda::import('pyapikdc.sal.ext.tm.saleOutStock')
      app <- mdl$SaleOutStockBill_TM(token = token_api_erpKdc)
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
#' sal_outStockServer()
sal_outStockServer <- function(input,output,session,dms_token,erp_token,apsToken) {
  sal_outStockSelectServer(input = input,output = output,session = session,dms_token = dms_token,erp_token=erp_token)


  sal_outStockupdateServer(input = input,output = output,session = session,dms_token = dms_token,erp_token=erp_token)

  sal_outStockSyncServer(input = input,output = output,session = session,dms_token = dms_token,erp_token=erp_token,apsToken=apsToken)

}















