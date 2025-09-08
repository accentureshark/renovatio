const { useState, useEffect } = React;

function App() {
  const [status, setStatus] = useState({ migratedFiles: 0, totalFiles: 0, errorCount: 0, progress: 0 });

  useEffect(() => {
    const fetchStatus = async () => {
      const res = await fetch('/api/migration/status');
      const data = await res.json();
      setStatus(data);
    };
    fetchStatus();
    const id = setInterval(fetchStatus, 1000);
    return () => clearInterval(id);
  }, []);

  const percent = Math.round((status.progress || 0) * 100);

  return (
    <div style={{ fontFamily: 'sans-serif', margin: '2rem' }}>
      <h1>Migration Progress</h1>
      <div style={{ border: '1px solid #ccc', width: '100%', height: '30px', marginBottom: '0.5rem' }}>
        <div style={{ width: `${percent}%`, backgroundColor: '#4caf50', height: '100%' }}></div>
      </div>
      <p>{percent}% complete</p>
      <p>{status.migratedFiles} / {status.totalFiles} files migrated</p>
      <p>Errors: {status.errorCount}</p>
    </div>
  );
}

ReactDOM.createRoot(document.getElementById('root')).render(<App />);
