const { test, expect } = require('@playwright/test');

test('create form manages tracks and looks up valid YouTube titles', async ({ page }) => {
  await page.route('https://www.youtube.com/oembed**', route => route.fulfill({
    status: 200,
    contentType: 'application/json',
    body: JSON.stringify({ title: 'Never Gonna Give You Up' }),
  }));

  await page.goto('/create');

  const trackRows = page.locator('#track-list > .track-row');
  await expect(trackRows).toHaveCount(1);

  await page.getByRole('button', { name: '+ Add a track' }).click();
  await expect(trackRows).toHaveCount(2);

  const firstRow = trackRows.nth(0);
  const secondRow = trackRows.nth(1);
  await secondRow.locator('.track-title').fill('Second song');

  await firstRow.locator('.video-url').fill('https://example.com/video');
  await expect(firstRow.locator('.url-hint')).toContainText('Use a youtube.com or youtu.be');
  await expect(firstRow.locator('.url-hint')).toHaveClass(/url-invalid/);

  await firstRow.locator('.video-url').fill('https://www.youtube.com/watch?v=dQw4w9WgXcQ');
  await expect(firstRow.locator('.url-hint')).toContainText('YouTube link looks good');
  await firstRow.locator('.video-url').dispatchEvent('change');
  await expect(firstRow.locator('.track-title')).toHaveValue('Never Gonna Give You Up');

  await firstRow.getByTitle('Move track down').click();
  await expect(trackRows.nth(0).locator('.track-title')).toHaveValue('Second song');
  await expect(trackRows.nth(1).locator('.track-title')).toHaveValue('Never Gonna Give You Up');

  await trackRows.nth(0).locator('[data-remove]').click();
  await expect(trackRows).toHaveCount(1);
  await expect(trackRows.first().locator('.track-title')).toHaveValue('Never Gonna Give You Up');

  await trackRows.first().locator('[data-remove]').click();
  await expect(trackRows).toHaveCount(1);
  await expect(trackRows.first().locator('.track-title')).toHaveValue('');

  await trackRows.first().locator('.track-title').fill('My Custom Title');
  await trackRows.first().locator('.video-url').fill('https://www.youtube.com/watch?v=dQw4w9WgXcQ');
  await trackRows.first().locator('.video-url').dispatchEvent('change');
  await expect(trackRows.first().locator('.url-hint')).toContainText('YouTube title found');
  await expect(trackRows.first().locator('.track-title')).toHaveValue('My Custom Title');

  await trackRows.first().locator('.track-note').fill('A song for the long way home');
  await page.locator('#tape-title').fill('Road trip songs');
  await page.getByRole('button', { name: 'Create mixtape' }).click();

  await expect(page.getByRole('heading', { name: 'Your mixtape is ready.' })).toBeVisible();
  await expect(page.locator('#share-link')).toHaveAttribute('value', /^\/m\/[A-Za-z0-9_-]+$/);
  await page.getByRole('link', { name: 'Preview your mixtape' }).click();
  await expect(page).toHaveURL(/\/m\/[A-Za-z0-9_-]+$/);
  await expect(page.getByRole('heading', { name: 'Road trip songs' })).toBeVisible();
  await expect(page.getByText('A song for the long way home')).toBeVisible();
});

test('mixtape player switches tracks via transport controls, queue items, and keyboard shortcuts', async ({ page }) => {
  await page.route('https://www.youtube.com/oembed**', route => route.fulfill({
    status: 200,
    contentType: 'application/json',
    body: JSON.stringify({ title: '' }),
  }));

  await page.goto('/create');
  await page.getByRole('button', { name: '+ Add a track' }).click();

  const trackRows = page.locator('#track-list > .track-row');
  await trackRows.nth(0).locator('.video-url').fill('https://www.youtube.com/watch?v=dQw4w9WgXcQ');
  await trackRows.nth(0).locator('.track-title').fill('Track One');
  await trackRows.nth(0).locator('.track-note').fill('Opening track note');

  await trackRows.nth(1).locator('.video-url').fill('https://www.youtube.com/watch?v=M7lc1UVf-VE');
  await trackRows.nth(1).locator('.track-title').fill('Track Two');
  await trackRows.nth(1).locator('.track-note').fill('Closing track note');

  await page.locator('#tape-title').fill('Two Track Tape');
  await page.getByRole('button', { name: 'Create mixtape' }).click();
  await page.getByRole('link', { name: 'Preview your mixtape' }).click();

  await expect(page.locator('#current-track-title')).toHaveText('Track One');
  await expect(page.locator('#current-track-note')).toHaveText('Opening track note');
  await expect(page.locator('#current-track-counter')).toHaveText('1 / 2');
  await expect(page.locator('#previous-track')).toBeDisabled();
  await expect(page.locator('#next-track')).toBeEnabled();

  await page.locator('#next-track').click();
  await expect(page.locator('#current-track-title')).toHaveText('Track Two');
  await expect(page.locator('#current-track-note')).toHaveText('Closing track note');
  await expect(page.locator('#current-track-counter')).toHaveText('2 / 2');
  await expect(page.locator('#previous-track')).toBeEnabled();
  await expect(page.locator('#next-track')).toBeDisabled();

  await page.keyboard.press('ArrowLeft');
  await expect(page.locator('#current-track-title')).toHaveText('Track One');
  await expect(page.locator('#current-track-counter')).toHaveText('1 / 2');

  await page.locator('#track-queue [data-track-index="1"]').click();
  await expect(page.locator('#current-track-title')).toHaveText('Track Two');
  await expect(page.locator('#current-track-counter')).toHaveText('2 / 2');
});
