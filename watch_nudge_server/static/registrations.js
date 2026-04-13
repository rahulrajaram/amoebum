(() => {
  const apiBase = (document.body.dataset.apiBase || "").replace(/\/+$/, "");
  const registrationsUrl = `${apiBase}/api/watches`;
  const statusEl = document.getElementById("status");
  const form = document.getElementById("registration-form");
  const rowsEl = document.getElementById("registration-rows");
  const state = { rows: [] };

  function registrationId(registration) {
    return String(
      registration.id ??
        registration.registration_id ??
        registration.watch_id ??
        "",
    );
  }

  function setStatus(message, kind = "ok") {
    statusEl.textContent = message;
    statusEl.className = `status ${kind}`;
  }

  function normalizeRows(payload) {
    if (Array.isArray(payload)) {
      return payload;
    }
    if (payload && payload.data && Array.isArray(payload.data.items)) {
      return payload.data.items;
    }
    if (payload && Array.isArray(payload.registrations)) {
      return payload.registrations;
    }
    return [];
  }

  function escapeHtml(value) {
    return String(value ?? "")
      .replaceAll("&", "&amp;")
      .replaceAll("<", "&lt;")
      .replaceAll(">", "&gt;")
      .replaceAll('"', "&quot;")
      .replaceAll("'", "&#39;");
  }

  async function requestJson(method, url, payload) {
    const response = await fetch(url, {
      method,
      headers: { "content-type": "application/json" },
      body: payload ? JSON.stringify(payload) : undefined,
    });

    const raw = await response.text();
    let data = null;
    if (raw) {
      try {
        data = JSON.parse(raw);
      } catch (_error) {
        data = { detail: raw };
      }
    }

    if (!response.ok) {
      const detail = data && (data.detail || data.error);
      throw new Error(detail || `${method} failed (${response.status})`);
    }
    return data;
  }

  async function refreshRows() {
    try {
      const payload = await requestJson("GET", registrationsUrl);
      state.rows = normalizeRows(payload);
      renderRows();
      setStatus(`Loaded ${state.rows.length} registration(s).`);
    } catch (error) {
      setStatus(error.message, "error");
    }
  }

  async function addRegistration(event) {
    event.preventDefault();

    const payload = {
      id: form.id.value.trim(),
      watch_path: form.watch_path.value.trim(),
      nudge_minutes: Number(form.nudge_minutes.value || 45),
      notes: form.notes.value.trim(),
    };

    if (!payload.id || !payload.watch_path) {
      setStatus("Registration ID and Watch Path are required.", "error");
      return;
    }

    try {
      await requestJson("POST", registrationsUrl, payload);
      form.reset();
      form.nudge_minutes.value = "45";
      await refreshRows();
      setStatus(`Added registration ${payload.id}.`);
    } catch (error) {
      setStatus(error.message, "error");
    }
  }

  function rowPayload(rowEl) {
    return {
      id: rowEl.dataset.id,
      watch_path: rowEl.querySelector(".watch-path").value.trim(),
      nudge_minutes: Number(rowEl.querySelector(".nudge-minutes").value || 45),
      notes: rowEl.querySelector(".notes").value.trim(),
    };
  }

  async function updateRegistration(id, rowEl) {
    const payload = rowPayload(rowEl);
    try {
      await requestJson(
        "PUT",
        `${registrationsUrl}/${encodeURIComponent(id)}`,
        payload,
      );
      setStatus(`Updated registration ${id}.`);
      await refreshRows();
    } catch (error) {
      setStatus(error.message, "error");
    }
  }

  async function removeRegistration(id) {
    if (!window.confirm(`Remove registration ${id}?`)) {
      return;
    }
    try {
      await requestJson("DELETE", `${registrationsUrl}/${encodeURIComponent(id)}`);
      setStatus(`Removed registration ${id}.`);
      await refreshRows();
    } catch (error) {
      setStatus(error.message, "error");
    }
  }

  function renderRows() {
    rowsEl.replaceChildren();

    if (state.rows.length === 0) {
      const emptyRow = document.createElement("tr");
      emptyRow.innerHTML = "<td colspan='5'>No registrations found.</td>";
      rowsEl.appendChild(emptyRow);
      return;
    }

    state.rows.forEach((registration) => {
      const id = registrationId(registration);
      const watchPath = escapeHtml(registration.watch_path || "");
      const nudgeMinutes = escapeHtml(registration.nudge_minutes || 45);
      const notes = escapeHtml(registration.notes || "");
      const safeId = escapeHtml(id);
      const row = document.createElement("tr");
      row.dataset.id = id;
      row.innerHTML = `
        <td>${safeId}</td>
        <td><input class="watch-path" value="${watchPath}" /></td>
        <td><input class="nudge-minutes" type="number" min="1" value="${nudgeMinutes}" /></td>
        <td><input class="notes" value="${notes}" /></td>
        <td class="actions">
          <button type="button" data-action="update">Update</button>
          <button type="button" data-action="remove">Remove</button>
        </td>
      `;
      rowsEl.appendChild(row);
    });
  }

  rowsEl.addEventListener("click", async (event) => {
    const button = event.target.closest("button[data-action]");
    if (!button) {
      return;
    }
    const rowEl = button.closest("tr");
    const id = rowEl.dataset.id;
    if (!id) {
      setStatus("Missing registration ID.", "error");
      return;
    }
    const action = button.dataset.action;
    if (action === "update") {
      await updateRegistration(id, rowEl);
    } else if (action === "remove") {
      await removeRegistration(id);
    }
  });

  form.addEventListener("submit", addRegistration);
  refreshRows();
})();
