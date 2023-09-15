let el = 
  let open Tyxml.Html in
    (head 
      (title (txt "Greeting")) 
      [ script ~a:[a_src "https://unpkg.com/htmx.org@1.9.4"] (txt "");
        script ~a:[a_src "https://cdn.tailwindcss.com?plugins=forms,typography,aspect-ratio"] (txt "");
        script (txt "
        tailwind.config = {
          theme: {
            extend: {
              colors: {
                clifford: '#da373d',
                'stone': {
                  850: '#23201E'
                }
              }
            }
          }
        }");
        style [
          txt "
          @import url('https://fonts.googleapis.com/css2?family=JetBrains+Mono&display=swap');
          @import url('https://fonts.googleapis.com/css2?family=Nunito+Sans:opsz@6..12&display=swap');

          .scrolling-sidebar {
            height: calc(100vh - 2rem); /* Adjust the height as needed */
            overflow-y: auto;
            position: fixed;
          }";
        ]
      ]
    )

