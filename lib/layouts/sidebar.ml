let el = 
  let open Tyxml.Html in
  div ~a:[
    a_class["bg-stone-850";"text-white";"col-span-1"];
  ][
    div ~a:[a_class["p-4";"scrolling-sidebar"];] [
      (h1 ~a:[a_class["text-2xl";"font-semibold"]] [txt "Sidebar"]);
      ul ~a:[a_class ["mt-4"]] [
        li ~a:[a_class ["mb-2"]] [a ~a:[a_href "#"; a_class ["hover:text-lime-600"]][txt "Dashboard"]];
        li ~a:[a_class ["mb-2"]] [a ~a:[a_href "#"; a_class ["hover:text-lime-600"]][txt "Products"]];
        li ~a:[a_class ["mb-2"]] [a ~a:[a_href "#"; a_class ["hover:text-lime-600"]][txt "Customers"]];
        li ~a:[a_class ["mb-2"]] [a ~a:[a_href "#"; a_class ["hover:text-lime-600"]][txt "Orders"]];
        li ~a:[a_class ["mb-2"]] [a ~a:[a_href "#"; a_class ["hover:text-lime-600"]][txt "Settings22"]];
        li ~a:[a_class ["mb-2"]] [a ~a:[a_href "#"; a_class ["hover:text-lime-600"]][txt "Dashboard"]];
        li ~a:[a_class ["mb-2"]] [a ~a:[a_href "#"; a_class ["hover:text-lime-600"]][txt "Products"]];
        li ~a:[a_class ["mb-2"]] [a ~a:[a_href "#"; a_class ["hover:text-lime-600"]][txt "Customers"]];
        li ~a:[a_class ["mb-2"]] [a ~a:[a_href "#"; a_class ["hover:text-lime-600"]][txt "Orders"]];
        li ~a:[a_class ["mb-2"]] [a ~a:[a_href "#"; a_class ["hover:text-lime-600"]][txt "Settings22"]]
      ]
    ]
  ]