(function() {
  const queue=Array.from(document.querySelectorAll('#track-queue [data-track-index]'));
  if(!queue.length)return;
  let current=0,player=null,ready=false;
  const deck=document.querySelector('#player-deck'),status=document.querySelector('#playback-status'),playButton=document.querySelector('#toggle-playback'),startButton=document.querySelector('#start-playback');
  function announce(message) {
    status.textContent=message
  }
  function showTrack() {
    const item=queue[current];
    document.querySelector('#current-track-title').textContent=item.dataset.trackTitle;
    document.querySelector('#current-track-artist').textContent=item.dataset.trackArtist||'';
    document.querySelector('#current-track-note').textContent=item.dataset.trackNote;
    document.querySelector('#current-track-counter').textContent=(current+1)+' / '+queue.length;
    queue.forEach((row,index)=> {
      const active=index===current;
      row.setAttribute('aria-current',active?'true':'false');
      row.querySelector('.queue-indicator').textContent=active?'♪':''
    }
    );
    document.querySelector('#previous-track').disabled=current===0;
    document.querySelector('#next-track').disabled=current===queue.length-1
  }
  function selectTrack(index) {
    if(index<0||index>=queue.length)return;
    current=index;
    showTrack();
    startButton.hidden=true;
    if(ready)player.loadVideoById(queue[current].dataset.videoId)
  }
  showTrack();
  queue.forEach(row=>row.addEventListener('click',()=>selectTrack(Number(row.dataset.trackIndex))));
  document.querySelector('#previous-track').addEventListener('click',()=> {
    if(current===0) {
      if(ready)player.seekTo(0,true);
      return
    }
    selectTrack(current-1)
  }
  );
  document.querySelector('#next-track').addEventListener('click',()=>selectTrack(current+1));
  function play() {
    if(ready) {
      player.playVideo();
      startButton.hidden=true;
      announce('Starting playback…')
    }
    
  }
  function toggle() {
    if(!ready)return;
    if(player.getPlayerState()===YT.PlayerState.PLAYING) {
      player.pauseVideo()
    }
    else play()
  }
  playButton.addEventListener('click',toggle);
  startButton.addEventListener('click',play);
  window.onYouTubeIframeAPIReady=function() {
    player=new YT.Player('youtube-player', {
      width:1280,height:720,videoId:queue[0].dataset.videoId,playerVars: {
        autoplay:1,controls:1,playsinline:1,rel:0,origin:window.location.origin
      }
      ,events: {
        onReady:function(event) {
          ready=true;
          announce('Starting the first track…');
          if(current===0)event.target.playVideo();
          else event.target.loadVideoById(queue[current].dataset.videoId)
        }
        ,onStateChange:function(event) {
          if(event.data===YT.PlayerState.PLAYING) {
            deck.dataset.playing='true';
            playButton.textContent='Pause';
            playButton.setAttribute('aria-pressed','true');
            startButton.hidden=true;
            announce('Now playing '+queue[current].dataset.trackTitle)
          }
          else if(event.data===YT.PlayerState.PAUSED) {
            deck.dataset.playing='false';
            playButton.textContent='Play';
            playButton.setAttribute('aria-pressed','false');
            announce('Paused')
          }
          else if(event.data===YT.PlayerState.ENDED) {
            deck.dataset.playing='false';
            if(current<queue.length-1)selectTrack(current+1);
            else {
              playButton.textContent='Play';
              playButton.setAttribute('aria-pressed','false');
              announce('You reached the end of the tape.')
            }
            
          }
          
        }
        ,onAutoplayBlocked:function() {
          deck.dataset.playing='false';
          playButton.textContent='Play';
          playButton.setAttribute('aria-pressed','false');
          startButton.hidden=false;
          announce('Your browser blocked autoplay. Press Start listening to play the tape.')
        }
        ,onError:function() {
          deck.dataset.playing='false';
          playButton.textContent='Play';
          playButton.setAttribute('aria-pressed','false');
          startButton.hidden=false;
          announce('This video cannot be played here. Try another track.')
        }
        
      }
      
    }
    );
    
  }
  ;
  const api=document.createElement('script');
  api.src='https://www.youtube.com/iframe_api';
  api.async=true;
  api.onerror=function() {
    announce('The YouTube player did not load. Check your connection and refresh to try again.')
  }
  ;
  document.head.appendChild(api)
}
)();
