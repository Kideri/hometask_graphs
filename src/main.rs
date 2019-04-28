#![allow(dead_code)]
#![allow(non_snake_case)]
#![allow(unused)]

#[derive(PartialEq, Eq, Clone, Hash, Debug)]
struct Graph {
    node_count: usize,
    edge_weights: Vec<Vec<Option<u32>>>,
}

impl Graph {
    fn new() -> Self {
        Graph {
            node_count: 0,
            edge_weights: vec![vec![]],
        }
    }

    fn variant() -> Self {
        Graph {
            node_count: 12,
            edge_weights: vec![
                "03 2 1544  1",
                "3032 3 4 544",
                " 30   2 4145",
                "22 0  1  12 ",
                "    0  41   ",
                "13   0     1",
                "5 21  044  5",
                "44  4 401254",
                "4 4 1 410 5 ",
                " 511   2 0  ",
                " 442   55 05",
                "145  154  50",
            ]
            .iter()
            .map(|s| s.chars().map(|c| c.to_string().parse().ok()).collect())
            .collect(),
        }
    }

    fn print_matrix(&self, mapper: fn(Option<u32>) -> String) {
        for i in 0..self.node_count {
            print!("&$e_{{{}}}$", i + 1);
        }
        println!(r"\\");
        for i in 0..self.node_count {
            print!("$e_{{{}}}$", i + 1);
            for j in 0..self.node_count {
                print!("&{}", mapper(self.edge_weights[i][j]));
            }
            println!(r"\\");
        }
    }

    fn print_weight_matrix(&self) {
        self.print_matrix(|o| match o {
            Some(num) => format!("{}", num),
            None => "".into(),
        });
    }

    fn print_edge_matrix(&self) {
        self.print_matrix(|o| if o.is_some() { "1" } else { "0" }.into());
    }

    fn print_graph(&self) {
        for i in 0..self.node_count {
            let phi = i as f64 / self.node_count as f64 * 2.0 * std::f64::consts::PI;
            let r = 4.0;
            let x = r * phi.sin();
            let y = r * phi.cos();
            println!(
                r"\node[draw,circle] (e{}) at ({},{}) {{$e_{{{}}}$}};",
                i,
                x,
                y,
                i + 1
            );
        }
        for i in 0..self.node_count {
            for j in 0..self.node_count {
                if i > j && self.edge_weights[i][j].is_some() {
                    println!(r"\path (e{}) edge (e{});", i, j);
                }
            }
        }
    }

    fn color_sorted_nodes(&self) {
        let mut node_degrees: Vec<_> = self
            .edge_weights
            .iter()
            .map(|v| v.iter().filter(|o| o.is_some()).count() - 1)
            .enumerate()
            .collect();
        node_degrees.sort_by_key(|&(_, c)| c);
        node_degrees.reverse();
        let node_degrees = node_degrees;

        let mut node_colors = vec![None; self.node_count];
        for color in 0.. {
            let unpainted_left = node_colors.iter().filter(|o| o.is_none()).count();
            if unpainted_left == 0 {
                break;
            }

            for i in 0..self.node_count {
                let nii = node_degrees[i].0;
                if node_colors[nii].is_some() {
                    continue;
                }
                print!("&$e_{{{}}}$", nii + 1);
            }
            println!(r"&$r_i$\\");

            for i in 0..self.node_count {
                let nii = node_degrees[i].0;
                if node_colors[nii].is_some() {
                    continue;
                }
                print!("$e_{{{}}}$", nii + 1);
                for j in 0..self.node_count {
                    let nij = node_degrees[j].0;
                    if node_colors[nij].is_some() {
                        continue;
                    }
                    print!("&");
                    if j >= i {
                        print!(
                            "{}",
                            if i != j && self.edge_weights[nii][nij].is_some() {
                                1
                            } else {
                                0
                            }
                        );
                    }
                }
                println!(r"&{}\\", node_degrees[i].1);
            }

            println!();
            print!("Color {}:;", color + 1);
            for i in 0..self.node_count {
                let nii = node_degrees[i].0;
                let mut can_paint = node_colors[nii].is_none();
                for j in 0..self.node_count {
                    let nij = node_degrees[j].0;
                    if self.edge_weights[nii][nij].is_some() && node_colors[nij] == Some(color) {
                        can_paint = false;
                    }
                }
                if can_paint {
                    node_colors[nii] = Some(color);
                    print!("$e_{{{}}}$;", nii + 1);
                }
            }
            println!(r"\\");
        }

        println!();
        println!(r"Node&Color\\");
        for i in 0..self.node_count {
            println!(r"$e_{{{}}}$&{}\\", i + 1, node_colors[i].unwrap() + 1);
        }
    }

    fn pathfind_dijkstra(&self, start: usize, finish: usize) {
        let mut shortest_path = vec![None; self.node_count];
        // (dist, from, final)
        shortest_path[start] = Some((0, start, false));
        let mut log = Vec::new();

        loop {
            log.push(shortest_path.clone());

            let mut p = None;
            for i in 0..self.node_count {
                match shortest_path[i] {
                    Some((dist_x, from_x, false)) => match p {
                        Some((_, dist_p, _)) if dist_x < dist_p => p = Some((i, dist_x, from_x)),
                        None => p = Some((i, dist_x, from_x)),
                        _ => {}
                    },
                    _ => {}
                }
            }

            // (node, dist, from)
            let p = match p {
                Some(val) => val,
                None => break,
            };

            shortest_path[p.0] = Some((p.1, p.2, true));

            for i in 0..self.node_count {
                if i == p.0 {
                    continue;
                }

                match self.edge_weights[p.0][i] {
                    Some(weight) => match shortest_path[i] {
                        Some((length, _, false)) if p.1 + weight < length => {
                            shortest_path[i] = Some((p.1 + weight, p.0, false))
                        }
                        None => shortest_path[i] = Some((p.1 + weight, p.0, false)),
                        _ => {}
                    },
                    _ => {}
                }
            }
        }

        for step in 0..log.len() {
            print!("&{}", step + 1);
        }
        println!(r"\\");

        for node in 0..self.node_count {
            print!("$e_{{{}}}$", node + 1);
            for step in 0..log.len() {
                match log[step][node] {
                    Some((length, _, true)) => match log[step - 1][node] {
                        Some((_, _, true)) => print!("&"),
                        _ => print!("&${}^+$", length),
                    },
                    Some((length, _, false)) => print!("&{}", length),
                    None => print!(r"&$\infty$"),
                }
            }
            println!(r"\\");
        }
        println!();

        for node in 0..self.node_count {
            let (dist, from, _) = shortest_path[node].unwrap();
            println!(
                r"$e_{{{}}}$&$e_{{{}}}$&{}&{}\\",
                node + 1,
                from + 1,
                self.edge_weights[node][from].unwrap(),
                dist
            );
        }
        println!();

        let mut path = Vec::new();
        let mut current_node = finish;
        while current_node != start {
            path.push(current_node);
            current_node = shortest_path[current_node].unwrap().1;
        }
        path.push(current_node);
        path.reverse();

        print!("$");
        for i in 1..path.len() {
            print!(
                r"e_{{{}}}\xrightarrow[{}]{{}} ",
                path[i - 1] + 1,
                self.edge_weights[path[i - 1]][path[i]].unwrap()
            );
        }
        println!("e_{{{}}}$", finish + 1);
    }

    fn frank_frish(&self, start: usize, finish: usize) {
        let mut collapsed_vertices: Vec<_> = (0..self.node_count).map(|i| vec![i]).collect();
        let mut collapsed_edges = Vec::new();
        for i in 0..self.node_count {
            for j in 0..self.node_count {
                if j <= i {
                    continue;
                }

                match self.edge_weights[i][j] {
                    Some(w) => collapsed_edges.push((i, j, w)),
                    _ => {}
                }
            }
        }
        let mut s = start;
        let mut t = finish;
        let mut log = Vec::new();

        loop {
            if s == t {
                break;
            }
            let Q = collapsed_edges
                .iter()
                .filter_map(|&(i, j, w)| if i == s || j == s { Some(w) } else { None })
                .max();
            let Q = match Q {
                Some(num) => num,
                _ => break,
            };
            let mut to_collapse = Vec::new();
            for &(i, j, w) in &collapsed_edges {
                if w >= Q {
                    to_collapse.push((i, j));
                }
            }

            to_collapse.sort();
            to_collapse.dedup();
            log.push((
                s,
                t,
                collapsed_vertices.clone(),
                collapsed_edges.clone(),
                Q,
                to_collapse.clone(),
            ));

            if s == t {
                break;
            }

            let mut collapse_into: Vec<_> = (0..collapsed_vertices.len()).collect();
            loop {
                let mut collapsed_something = false;
                for &(i, j) in &to_collapse {
                    let gi = collapse_into[i];
                    let gj = collapse_into[j];
                    let min = if gi < gj { gi } else { gj };
                    if collapse_into[i] != min || collapse_into[j] != min {
                        collapse_into[i] = min;
                        collapse_into[j] = min;
                        collapsed_something = true;
                    }
                }
                if !collapsed_something {
                    break;
                }
            }

            let mut to_shrink = Vec::new();
            for i in 0..collapse_into.len() {
                let mut to_push = Vec::new();
                for j in 0..collapse_into.len() {
                    if collapse_into[j] == i {
                        to_push.push(j);
                    }
                }
                if !to_push.is_empty() {
                    to_shrink.push(to_push);
                }
            }

            let mut new_collapsed_vertices = Vec::new();
            for i in 0..to_shrink.len() {
                let mut to_push = Vec::new();
                for &j in &to_shrink[i] {
                    if s == j {
                        s = i;
                    }
                    if t == j {
                        t = i;
                    }
                    for k in 0..collapsed_edges.len() {
                        if collapsed_edges[k].0 == j {
                            collapsed_edges[k].0 = i;
                        }
                        if collapsed_edges[k].1 == j {
                            collapsed_edges[k].1 = i;
                        }
                        if collapsed_edges[k].0 > collapsed_edges[k].1 {
                            let tmp = collapsed_edges[k].0;
                            collapsed_edges[k].0 = collapsed_edges[k].1;
                            collapsed_edges[k].1 = tmp;
                        }
                    }
                    for &k in &collapsed_vertices[j] {
                        to_push.push(k);
                    }
                }
                to_push.sort();
                new_collapsed_vertices.push(to_push);
            }
            collapsed_vertices = new_collapsed_vertices;

            collapsed_edges = collapsed_edges
                .iter()
                .filter_map(|&(i, j, w)| if i != j { Some((i, j, w)) } else { None })
                .collect();
            collapsed_edges.sort();
            collapsed_edges.reverse();
            collapsed_edges.dedup_by_key(|&mut (i, j, _)| (i, j));
        }

        for step in 0..log.len() {
            let (s, t, vertices, edges, Q, to_collapse) = &log[step];
            let (s, t, Q) = (*s, *t, *Q);

            let node_names: Vec<String> = vertices
                .iter()
                .map(|v| {
                    v.iter()
                        .map(|&i| format!("e_{{{}}}", i + 1))
                        .fold("".into(), |a, b| format!("{},{}", a, b))
                        .chars()
                        .skip(1)
                        .collect()
                })
                .collect();
            let paren_node_names: Vec<_> = node_names
                .iter()
                .enumerate()
                .map(|(i, s)| {
                    if vertices[i].len() == 1 {
                        s.clone()
                    } else {
                        format!("({})", s.clone())
                    }
                })
                .collect();
            println!(r"\bigskip");
            println!(r"\noindent");
            println!(r"\begin{{minipage}}{{\textwidth}}");
            println!(r"Шаг {}\\", step + 1);

            println!(r"\begin{{table}}[H]");
            println!(r"\centering");
            println!(r"\caption{{Граф на шаге {}}}", step + 1);
            println!(r"\begin{{tabular}}{{r*{{{}}}{{|c}}}}", vertices.len());
            for i in 0..vertices.len() {
                print!(r"&${}$", paren_node_names[i]);
            }
            println!(r"\\");
            for i in 0..vertices.len() {
                print!(r"\hline ${}$", paren_node_names[i]);
                for j in 0..vertices.len() {
                    print!("&");
                    for &(e1, e2, w) in edges.iter() {
                        if (e1 == i && e2 == j) || (e1 == j && e2 == i) {
                            print!("{}", w);
                            break;
                        }
                    }
                }
                println!(r"\\");
            }
            println!(r"\end{{tabular}}");
            println!(r"\end{{table}}");

            println!(r"Ребра сечения $K_{{{}}}$:\\", step + 1);
            for &(e1, e2, w) in edges.iter() {
                if e1 == s || e2 == s {
                    println!(
                        r"\mbox{{$({};{})$ длины {}}},",
                        paren_node_names[e1], paren_node_names[e2], w
                    );
                }
            }
            println!(r"\\");
            println!(r"$Q={}$\\", Q);

            println!(r"Слияние вершин:\\");
            for &(e1, e2) in to_collapse.iter() {
                println!(r"\mbox{{$({},{})$}},", node_names[e1], node_names[e2]);
            }
            println!(r"\end{{minipage}}");
        }
    }

    fn gamilton_recursion(
        &self,
        mut S: &mut Vec<usize>,
        mut included: &mut Vec<bool>,
        node: usize,
    ) -> bool {
        if !S.is_empty() {
            print!(r"\textquotedbl Возможная\textquotedbl{{}} вершина $e_{{{}}}\in\Gamma e_{{{}}}$, $S=\{{", node + 1, S.last().unwrap() + 1);
            for i in 0..S.len() {
                print!("e_{{{}}},", S[i] + 1);
            }
            println!(r"e_{{{}}}\}}$\\", node + 1);
        }
        S.push(node);
        included[node] = true;

        if included.iter().all(|b| *b) {
            if self.edge_weights[S[0]][node].is_some() {
                return true;
            } else {
                println!(r"Ребра $(e_{{{}}};e_{{{}}})$ нет. Удалим вершину $e_{{{}}}$\\", node + 1, S[0] + 1, node + 1);
                return false;
            }
        }

        for i in (0..self.node_count) {
            let i = (i + 2) % self.node_count;
            if self.edge_weights[node][i].is_none() || included[i] {
                continue;
            }
            if self.gamilton_recursion(&mut S, &mut included, i) {
                return true;
            }
        }

        println!(r"У $e_{{{}}}$ больше нет \textquotedbl возможных\textquotedbl{{}} вершин. Удалим её\\", node + 1);
        included[node] = false;
        S.pop().unwrap();
        false
    }

    fn gamilton_cycle(&self) {
        let mut S = Vec::new();
        let mut included = vec![false; self.node_count];
        println!(r"Включаем в S начальную вершину $S=\{{e_1\}}$\\");
        self.gamilton_recursion(&mut S, &mut included, 0);
        println!(r"Итоговый гамильтонов цикл: $S=\{{");
        for i in 0..self.node_count {
            if i != 0 {
                print!(",");
            }
            print!("e_{{{}}}", S[i] + 1);
        }
        println!(r"\}}$\\");
        println!();
        self.print_graph();
        println!();
        for i in 0..self.node_count {
            let j = (i + 1) % self.node_count;
            println!(r"\path [line width=0.5mm] (e{}) edge (e{});", S[i], S[j]);
        }
        println!();
    }
}

fn main() {
    let variant = Graph::variant();
    // variant.print_edge_matrix();
    // println!();
    // variant.print_graph();
    // println!();
    variant.gamilton_cycle();
}
