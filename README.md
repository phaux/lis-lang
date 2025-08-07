# Lis 🦊 Lang

A vibe-coded scripting language.

<pre><code><span style="color:orchid">let</span> <span style="color:tomato">me</span> = {
    <span style="color:goldenrod">name</span>: <span style="color:mediumseagreen">"lis"</span>,
    <span style="color:goldenrod">age</span>: <span style="color:chocolate">1</span>,
};

<span style="color:orchid">fn</span> <span style="color:dodgerblue">greet</span>(<span style="color:tomato">person</span>) <span style="color:orchid">do</span> {
    <span style="color:orchid">if</span> <span style="color:tomato">person</span>.<span style="color:goldenrod">age</span> < <span style="color:chocolate">30</span> <span style="color:orchid">then</span> {
        <span style="color:orchid">print</span> <span style="color:mediumseagreen">"sup "</span> ++ <span style="color:tomato">person</span>.<span style="color:goldenrod">name</span>;
    }
    <span style="color:orchid">else</span> {
        <span style="color:orchid">print</span> <span style="color:mediumseagreen">"hello "</span> ++ <span style="color:tomato">person</span>.<span style="color:goldenrod">name</span>;
    }
}

<span style="color:dodgerblue">greet</span>(<span style="color:tomato">me</span>);
</code></pre>
