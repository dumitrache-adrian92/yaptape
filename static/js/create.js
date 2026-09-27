function youtubeVideoId(value) {
  try {
    const url=new URL(value),host=url.hostname.toLowerCase(),parts=url.pathname.split('/').filter(Boolean);
    let id='';
    if(['youtube.com','www.youtube.com','m.youtube.com','music.youtube.com'].includes(host)) {
      if(parts[0]==='watch')id=url.searchParams.get('v')||'';
      else if(['shorts','embed','live'].includes(parts[0]))id=parts[1]||''
    }
    else if(['youtu.be','www.youtu.be'].includes(host))id=parts[0]||'';
    return ['https:','http:'].includes(url.protocol)&&/^[A-Za-z0-9_-]{11}$/.test(id)?id:''
  }
  catch(_) {
    return ''
  }
  
}
const pendingTitleLookups=new Map();
function lookupTitle(input,id) {
  const row=input.closest('.track-row'),titleField=row.querySelector('.track-title'),hint=row.querySelector('.url-hint'),version=Number(input.dataset.lookupVersion||0)+1;
  input.dataset.lookupVersion=String(version);
  input.dataset.lookupUrl=input.value;
  titleField.value='';
  hint.textContent='Looking up YouTube title…';
  const endpoint=new URL('https://www.youtube.com/oembed');
  endpoint.searchParams.set('url','https://www.youtube.com/watch?v='+id);
  endpoint.searchParams.set('format','json');
  let request=fetch(endpoint, {
    mode:'cors',credentials:'omit',signal:AbortSignal.timeout(8000)
  }
  ).then(response=> {
    if(!response.ok)throw new Error('Title lookup failed');
    return response.json()
  }
  ).then(data=> {
    if(Number(input.dataset.lookupVersion)!==version)return;
    if(typeof data.title==='string'&&data.title.trim()) {
      titleField.value=data.title.trim();
      hint.textContent='YouTube title found. You can edit it.'
    }
    else {
      hint.textContent='Title unavailable; you can enter one yourself.'
    }
    
  }
  ).catch(()=> {
    if(Number(input.dataset.lookupVersion)===version)hint.textContent='Could not load the title; you can enter one yourself.'
  }
  ).finally(()=> {
    if(pendingTitleLookups.get(input)===request)pendingTitleLookups.delete(input)
  }
  );
  pendingTitleLookups.set(input,request);
  return request
}
document.addEventListener('input',function(event) {
  if(!event.target.matches('.video-url'))return;
  const input=event.target,row=input.closest('.track-row'),titleField=row.querySelector('.track-title'),hint=row.querySelector('.url-hint');
  if(input.value!==input.dataset.lookupUrl)titleField.value='';
  hint.textContent=input.value?(youtubeVideoId(input.value)?'YouTube link looks good.':'Use a youtube.com or youtu.be video link.'):'YouTube links from youtube.com or youtu.be';
  hint.classList.toggle('url-valid',Boolean(input.value&&youtubeVideoId(input.value)));
  hint.classList.toggle('url-invalid',Boolean(input.value&&!youtubeVideoId(input.value)))
}
);
document.addEventListener('change',function(event) {
  if(!event.target.matches('.video-url'))return;
  const id=youtubeVideoId(event.target.value);
  if(id)lookupTitle(event.target,id)
}
);
const createForm=document.querySelector('form[action="/create"]');
createForm.addEventListener('submit',function(event) {
  const inputs=Array.from(createForm.querySelectorAll('.video-url'));
  const requests=[];
  for(const input of inputs) {
    const id=youtubeVideoId(input.value);
    if(id&&input.dataset.lookupUrl!==input.value)requests.push(lookupTitle(input,id))
  }
  requests.push(...pendingTitleLookups.values());
  if(requests.length) {
    event.preventDefault();
    Promise.all(requests).then(()=>createForm.requestSubmit(event.submitter))
  }
  
}
);
document.addEventListener('click',function(event) {
  const add=event.target.closest('#add-track');
  if(add) {
    const list=document.querySelector('#track-list'),row=document.querySelector('#track-template').content.firstElementChild.cloneNode(true);
    list.append(row);
    row.querySelector('input').focus();
    return
  }
  const move=event.target.closest('[data-move]');
  if(move) {
    const row=move.closest('.track-row');
    if(move.dataset.move==='up'&&row.previousElementSibling)row.parentNode.insertBefore(row,row.previousElementSibling);
    if(move.dataset.move==='down'&&row.nextElementSibling)row.parentNode.insertBefore(row.nextElementSibling,row);
    return
  }
  const remove=event.target.closest('[data-remove]');
  if(remove) {
    const rows=document.querySelectorAll('#track-list .track-row');
    if(rows.length>1)remove.closest('.track-row').remove()
  }
  
}
);
document.body.addEventListener('htmx:responseError',function() {
  document.querySelector('#create-flow').insertAdjacentHTML('afterbegin','<p class=error role=alert>We could not save this mixtape. Please try again.</p>')
}
);
