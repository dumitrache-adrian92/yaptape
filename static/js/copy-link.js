const input=document.querySelector('#share-link');
input.value=new URL(input.value,location.origin).href;
document.addEventListener('click',async function(e) {
  const button=e.target.closest('[data-copy]');
  if(!button)return;
  try {
    await navigator.clipboard.writeText(input.value);
    document.querySelector('#copy-status').textContent='Link copied to clipboard.'
  }
  catch(_) {
    input.select();
    document.execCommand('copy');
    document.querySelector('#copy-status').textContent='Link copied.'
  }
  
}
);
