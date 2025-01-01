import { useState } from 'react';

export default function Home() {
  const [formData, setFormData] = useState({
    Sex: "",
    Red: "",
    Green: "",
    Blue: "",
    Hb: "",
  });
  const [prediction, setPrediction] = useState(null);
  const [error, setError] = useState(null);

  const handleChange = (e) => {
    const { name, value } = e.target;
    setFormData({ ...formData, [name]: value });
  };

  const handleSubmit = async (e) => {
    e.preventDefault();
    try {
      const res = await fetch("http://<ecs-ip-address>:8888/predict", {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify(formData),
      });
      const data = await res.json();
      if (data.success) {
        setPrediction(data.prediction);
        setError(null);
      } else {
        setError(data.message);
      }
    } catch (err) {
      setError("Error connecting to backend.");
    }
  };

  return (
    <div style={{ padding: "20px" }}>
      <h1>Anaemia Prediction</h1>
      <form onSubmit={handleSubmit}>
        <input
          name="Sex"
          type="text"
          placeholder="Sex"
          value={formData.Sex}
          onChange={handleChange}
          required
        />
        <input
          name="Red"
          type="number"
          placeholder="Red Pixel %"
          value={formData.Red}
          onChange={handleChange}
          required
        />
        <input
          name="Green"
          type="number"
          placeholder="Green Pixel %"
          value={formData.Green}
          onChange={handleChange}
          required
        />
        <input
          name="Blue"
          type="number"
          placeholder="Blue Pixel %"
          value={formData.Blue}
          onChange={handleChange}
          required
        />
        <input
          name="Hb"
          type="number"
          placeholder="Hb Level"
          value={formData.Hb}
          onChange={handleChange}
          required
        />
        <button type="submit">Predict</button>
      </form>
      {prediction && <h3>Prediction: {prediction}</h3>}
      {error && <h3 style={{ color: "red" }}>{error}</h3>}
    </div>
  );
}

