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
  await firstRow.locator('.track-title').fill('First song');
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
