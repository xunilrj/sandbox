document.addEventListener("click", x => {
    console.log(x.target.dataset, x.target.parentElement.dataset);
    const iframe = document.getElementById("view");
    if(x.target.dataset.url){    
      iframe.src = x.target.dataset.url;
    }
    if(x.target.parentElement.dataset.url){    
      iframe.src = x.target.parentElement.dataset.url;
    }
  });