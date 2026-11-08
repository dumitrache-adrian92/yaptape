function loadYouTubeSpec() {
  const el=document.querySelector('#create-flow[data-youtube-spec]');
  return el?JSON.parse(el.dataset.youtubeSpec):null
}
function youtubeVideoId(value) {
  try {
    const spec=loadYouTubeSpec();
    if(!spec)return '';
    const url=new URL(value),host=url.hostname.toLowerCase(),parts=url.pathname.split('/').filter(Boolean);
    let id='';
    if(spec.specPrimaryHosts.includes(host)) {
      if(parts[0]===spec.specWatchPath)id=url.searchParams.get(spec.specWatchParam)||'';
      else if(spec.specPathPrefixes.includes(parts[0]))id=parts[1]||''
    }
    else if(spec.specShortHosts.includes(host))id=parts[0]||'';
    return spec.specSchemes.includes(url.protocol)&&new RegExp(spec.specIdPattern).test(id)?id:''
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
  if(titleField.value===titleField.dataset.autoFilled)titleField.value='';
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
      const fetchedTitle=data.title.trim();
      if(!titleField.value.trim()||titleField.value===titleField.dataset.autoFilled) {
        titleField.value=fetchedTitle;
        titleField.dataset.autoFilled=fetchedTitle
      }
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
  if(input.value!==input.dataset.lookupUrl) {
    input.dataset.lookupVersion=String(Number(input.dataset.lookupVersion||0)+1);
    if(titleField.value===titleField.dataset.autoFilled)titleField.value=''
  }
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
document.addEventListener('submit',function(event) {
  if(!event.target.matches('form[action="/create"]'))return;
  const form=event.target;
  const inputs=Array.from(form.querySelectorAll('.video-url'));
  const requests=[];
  for(const input of inputs) {
    const id=youtubeVideoId(input.value);
    if(id&&input.dataset.lookupUrl!==input.value)requests.push(lookupTitle(input,id))
  }
  requests.push(...pendingTitleLookups.values());
  if(requests.length) {
    event.preventDefault();
    event.stopImmediatePropagation();
    Promise.all(requests).then(()=>form.requestSubmit(event.submitter))
  }
  
}
,true);
document.addEventListener('click',function(event) {
  if(!(event.target instanceof Element))return;
  const currentTrackList=document.querySelector('#track-list');
  const add=event.target.closest('#add-track');
  if(add&&currentTrackList) {
    const row=document.querySelector('#track-template').content.firstElementChild.cloneNode(true);
    currentTrackList.append(row);
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
    event.preventDefault();
    event.stopPropagation();

    const row=remove.closest('.track-row');
    if(!currentTrackList||!row||row.parentElement!==currentTrackList)return;

    if(currentTrackList.querySelectorAll('.track-row').length<=1) {
      const freshRow=document.querySelector('#track-template').content.firstElementChild.cloneNode(true);
      currentTrackList.replaceChild(freshRow,row);
      freshRow.querySelector('input').focus();
      return
    }
    currentTrackList.removeChild(row)
  }
  
}
);
document.body.addEventListener('htmx:beforeSwap',function(evt) {
  if(evt.detail.xhr&&evt.detail.xhr.status===422) {
    evt.detail.shouldSwap=true;
    evt.detail.isError=false
  }
}
);
document.body.addEventListener('htmx:responseError',function() {
  const flow=document.querySelector('#create-flow');
  if(!flow)return;
  flow.querySelectorAll(':scope > .error').forEach(el=>el.remove());
  flow.insertAdjacentHTML('afterbegin','<p class=error role=alert>We could not save this mixtape. Please try again.</p>')
}
);
