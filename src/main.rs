#![allow(dead_code)]
#![allow(non_snake_case)]
#![allow(unused)]

#[derive(PartialEq, Eq, Clone, Hash, Debug)]
struct Graph {
    node_count: usize,
    edge_weights: Vec<Vec<Option<u32>>>,
}

impl Graph {
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

    fn standard_naming(i: usize) -> String {
        format!("e_{{{}}}", i + 1)
    }

    fn mapper_weight(weight: Option<u32>) -> String {
        match weight {
            Some(num) => format!("{}", num),
            None => "".into(),
        }
    }

    fn mapper_existence(weight: Option<u32>) -> String {
        match weight {
            Some(_) => "1",
            None => "0",
        }
        .into()
    }

    fn mapper_space(weight: Option<u32>) -> String {
        match weight {
            Some(_) => "1",
            None => "",
        }
        .into()
    }

    fn print_matrix(
        &self,
        naming: impl Fn(usize) -> String,
        mapper: impl Fn(Option<u32>) -> String,
    ) {
        for i in 0..self.node_count {
            print!("&${}$", naming(i));
        }
        println!(r"\\");
        for i in 0..self.node_count {
            print!("${}$", naming(i));
            for j in 0..self.node_count {
                print!("&{}", mapper(self.edge_weights[i][j]));
            }
            println!(r"\\");
        }
    }

    fn print_triangle_matrix(
        &self,
        naming: impl Fn(usize) -> String,
        mapper: impl Fn(Option<u32>) -> String,
    ) {
        for i in 0..self.node_count {
            print!("&${}$", naming(i));
        }
        println!(r"\\");
        for i in 0..self.node_count {
            print!("${}$", naming(i));
            for j in 0..self.node_count {
                print!("&");
                if i < j {
                    print!("{}", mapper(self.edge_weights[i][j]));
                }
            }
            println!(r"\\");
        }
    }

    fn print_node_circle(&self, naming: impl Fn(usize) -> String) {
        for i in 0..self.node_count {
            let phi = i as f64 / self.node_count as f64 * 2.0 * std::f64::consts::PI;
            let r = 4.0;
            let x = r * phi.sin();
            let y = r * phi.cos();
            println!(
                r"\node[draw,circle] (x{}) at ({},{}) {{${}$}};",
                i,
                x,
                y,
                naming(i)
            );
        }
    }

    fn print_edges(&self) {
        for i in 0..self.node_count {
            for j in 0..self.node_count {
                if i < j && self.edge_weights[i][j].is_some() {
                    println!(r"\path (x{}) edge (x{});", i, j);
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
                print!("&${}$", Self::standard_naming(i));
            }
            println!(r"&$r_i$\\");

            for i in 0..self.node_count {
                let nii = node_degrees[i].0;
                if node_colors[nii].is_some() {
                    continue;
                }
                print!("${}$", Self::standard_naming(nii));
                for j in 0..self.node_count {
                    let nij = node_degrees[j].0;
                    if node_colors[nij].is_some() {
                        continue;
                    }
                    print!("&");
                    if i <= j {
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
                    print!("${}$;", Self::standard_naming(nii));
                }
            }
            println!(r"\\");
        }

        println!();
        println!(r"Node&Color\\");
        for i in 0..self.node_count {
            println!(
                r"${}$&{}\\",
                Self::standard_naming(i),
                node_colors[i].unwrap() + 1
            );
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
                    None => {}
                }
            }
        }

        for step in 0..log.len() {
            print!("&{}", step + 1);
        }
        println!(r"\\");

        for node in 0..self.node_count {
            print!("${}$", Graph::standard_naming(node));
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
                r"${}$&${}$&{}&{}\\",
                Self::standard_naming(node),
                Self::standard_naming(from),
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
                r"{}\xrightarrow[{}]{{}} ",
                Self::standard_naming(path[i - 1]),
                self.edge_weights[path[i - 1]][path[i]].unwrap()
            );
        }
        println!("{}$", Self::standard_naming(finish));
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
                    None => {}
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
                None => break,
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
                        .map(|&i| format!("{}", Self::standard_naming(i)))
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
        mut included: &mut [bool],
        node: usize,
    ) -> bool {
        S.push(node);
        included[node] = true;
        if !S.is_empty() {
            println!(r"\textquotedbl Возможная\textquotedbl{{}} вершина");
            print!(
                r"${}\in\Gamma {}$, $S=\{{",
                Self::standard_naming(node),
                Self::standard_naming(*S.last().unwrap())
            );
            for i in 0..S.len() - 1 {
                print!("{},", Self::standard_naming(S[i]));
            }
            println!(r"{}\}}$\\", Self::standard_naming(node));
        }

        let result = self.gamilton_recursion_unwrapped(&mut S, &mut included, node);
        if !result {
            included[node] = false;
            S.pop().unwrap();
            println!(
                r"Удалим вершину ${}$\\",
                Self::standard_naming(node)
            );
        }
        result
    }

    fn gamilton_recursion_unwrapped(
        &self,
        mut S: &mut Vec<usize>,
        mut included: &mut [bool],
        node: usize,
    ) -> bool {
        if included.iter().all(|b| *b) {
            if self.edge_weights[S[0]][node].is_some() {
                return true;
            } else {
                print!(
                    "Ребра $({};{})$ нет. ",
                    Self::standard_naming(node),
                    Self::standard_naming(S[0])
                );
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

        print!(
            r"У ${}$ больше нет \textquotedbl возможных\textquotedbl{{}} вершин. ",
            Self::standard_naming(node)
        );
        false
    }

    fn plane_families(
        mut full: &mut Vec<Vec<usize>>,
        mut stack: &mut Vec<usize>,
        bitmasks: &[u32],
        M: u32,
        iter: usize,
    ) {
        stack.push(iter - 1);
        Self::plane_families_unwrapped(&mut full, &mut stack, bitmasks, M, iter);
        stack.pop().unwrap();
    }

    fn plane_families_unwrapped(
        mut full: &mut Vec<Vec<usize>>,
        mut stack: &mut Vec<usize>,
        bitmasks: &[u32],
        M: u32,
        iter: usize,
    ) {
        for i in 0..stack.len() {
            if i != 0 {
                print!(", ");
            }
            print!("{}", stack[i] + 1);
        }
        print!(r"&\texttt{{");

        for i in 0..bitmasks.len() {
            print!("{}", (M >> i) & 1);
        }
        print!(r"}}&");

        let unset_bits: Vec<_> = (0..bitmasks.len())
            .filter(|i| ((M >> i) & 1) == 0)
            .collect();

        for i in 0..unset_bits.len() {
            if i != 0 {
                print!(", ");
            }
            print!("{}", unset_bits[i] + 1);
        }
        println!(r"\\");

        for &i in &unset_bits {
            if i < iter {
                continue;
            }
            Self::plane_families(&mut full, &mut stack, bitmasks, M | bitmasks[i], i + 1);
        }

        if unset_bits.is_empty() {
            full.push(stack.clone());
        }
    }

    fn intersection_matrix(psi: &Vec<Vec<usize>>) -> Vec<Vec<usize>> {
        let mut result = Vec::new();
        for i in 0..psi.len() {
            let mut to_push = Vec::new();
            for j in 0..psi.len() {
                let mut it1 = 0;
                let mut it2 = 0;
                let mut diff = 0;
                while it1 < psi[i].len() && it2 < psi[j].len() {
                    diff += 1;
                    let val1 = psi[i][it1];
                    let val2 = psi[j][it2];
                    if val1 <= val2 {
                        it1 += 1;
                    }
                    if val2 <= val1 {
                        it2 += 1;
                    }
                }
                while it1 < psi[i].len() {
                    diff += 1;
                    it1 += 1;
                }
                while it2 < psi[j].len() {
                    diff += 1;
                    it2 += 1;
                }
                to_push.push(diff);
            }
            result.push(to_push);
        }
        result
    }

    fn gamilton_cycle(&self) {
        let mut S = Vec::new();
        println!(r"Включаем в S начальную вершину $S=\{{e_1\}}$\\");
        self.gamilton_recursion(&mut S, &mut vec![false; self.node_count], 0);

        print!(r"Итоговый гамильтонов цикл: $S=\{{");
        for i in 0..self.node_count {
            if i != 0 {
                print!(",");
            }
            print!("{}", Self::standard_naming(S[i]));
        }
        println!(r"\}}$\\");
        println!();

        self.print_node_circle(Self::standard_naming);
        println!();
        for i in 0..self.node_count {
            let j = (i + 1) % self.node_count;
            println!(r"\path (x{}) edge (x{});", S[i], S[j]);
        }
        println!();

        let mut new_index = vec![0; self.node_count];
        for i in 0..self.node_count {
            new_index[S[i]] = i;
        }

        print!("до перенумерации");
        for i in 0..self.node_count {
            print!("&${}$", Self::standard_naming(i));
        }
        println!(r"\\");
        print!("после перенумерации");
        for i in 0..self.node_count {
            print!("&${}$", Self::standard_naming(new_index[i]));
        }
        println!();
        println!();

        let mut renumbered = Graph {
            node_count: self.node_count,
            edge_weights: vec![vec![None; self.node_count]; self.node_count],
        };

        for i in 0..self.node_count {
            for j in 0..self.node_count {
                renumbered.edge_weights[i][j] = self.edge_weights[S[i]][S[j]];
            }
        }

        for i in 0..self.node_count {
            print!("&${}$", Self::standard_naming(i));
        }
        println!(r"\\");

        for i in 0..self.node_count {
            print!("${}$", Self::standard_naming(i));
            for j in 0..self.node_count {
                match j {
                    j if j < i => print!("&"),
                    j if j == i => print!("&0"),
                    j if j == i + 1 => print!(r"&$\times$"),
                    j => match renumbered.edge_weights[i][j] {
                        Some(_) => print!("&1"),
                        None => print!("&"),
                    },
                }
            }
            println!(r"\\");
        }

        println!();
        renumbered.print_node_circle(Self::standard_naming);
        println!();
        renumbered.print_edges();
        println!();

        let mut edges = Vec::new();
        for i in 0..self.node_count {
            for j in 0..self.node_count {
                if j <= i + 1 {
                    continue;
                }

                if (j + 1) % self.node_count <= i {
                    continue;
                }

//                if j <= i + 2 {
//                    continue;
//                }
//
//                if (j + 2) % self.node_count <= i {
//                    continue;
//                }

                if renumbered.edge_weights[i][j].is_some() {
                    edges.push((i, j));
                }
            }
        }

        let edges = &edges[..15];

        let mut intersection_graph = Graph {
            node_count: edges.len(),
            edge_weights: vec![vec![None; edges.len()]; edges.len()],
        };

        for i in 0..edges.len() {
            for j in 0..edges.len() {
                let (a, b) = edges[i];
                let (c, d) = edges[j];
                if i == j || (a < c && c < b && b < d) {
                    intersection_graph.edge_weights[i][j] = Some(0);
                    intersection_graph.edge_weights[j][i] = Some(0);
                }
            }
        }

        for i in 0..edges.len() {
            print!(r"&$u_{{{},{}}}$", edges[i].0 + 1, edges[i].1 + 1);
        }
        println!(r"\\");
        for i in 0..edges.len() {
            print!(r"&{}", i + 1);
        }
        println!(r"\\");
        println!();

        intersection_graph.print_matrix(|i| format!("{}", i + 1), Self::mapper_space);
        println!();

        let mut stack = Vec::new();
        let bitmasks: Vec<_> = (0..edges.len())
            .map(|i| {
                (0..edges.len())
                    .filter_map(|j| intersection_graph.edge_weights[i][j].map(|_| (1 << j)))
                    .fold(0, |a, b| a | b)
            })
            .collect();
        let mut psi = Vec::new();
        Self::plane_families_unwrapped(&mut psi, &mut stack, bitmasks.as_slice(), 0, 0);
        let mut edge_used = vec![false; edges.len()];

        println!();
        for i in 0..psi.len() {
            print!(r"$\psi_{{{}}}=\{{", i + 1);
            for j in 0..psi[i].len() {
                if j != 0 {
                    print!(",");
                }
                print!(
                    r"u_{{{},{}}}",
                    edges[psi[i][j]].0 + 1,
                    edges[psi[i][j]].1 + 1
                );
                //                print!(r"r_{{{}}}", psi[i][j] + 1);
            }
            println!(r"\}}$\\");
        }

        let diff_mat = Self::intersection_matrix(&psi);
        println!();

        let mut alpha_max = 0;
        let mut maximal_pairs = Vec::new();

        for i in 0..psi.len() {
            print!("&{}", i + 1);
        }
        println!(r"\\");
        for i in 0..psi.len() {
            print!("{}", i + 1);
            for j in 0..psi.len() {
                if j < i {
                    print!("&");
                } else if j == i {
                    print!(r"&$\times$");
                } else {
                    if diff_mat[i][j] > alpha_max {
                        alpha_max = diff_mat[i][j];
                        maximal_pairs.clear();
                    }
                    if diff_mat[i][j] == alpha_max {
                        maximal_pairs.push((i, j));
                    }
                    print!("&{}", diff_mat[i][j]);
                }
            }
            println!(r"\\");
        }

        println!();
        print!(r"$max \alpha_{{\gamma\delta}}");
        for (i, j) in &maximal_pairs {
            print!(r"=\alpha_{{{},{}}}", i + 1, j + 1)
        }
        println!(r"={}$\\", alpha_max);
        println!("Возьмем ");
        for psi_id in &[maximal_pairs[0].0, maximal_pairs[0].1] {
            let psi_id = *psi_id;
            print!(r"$\psi_{{{}}}=\{{", psi_id + 1);
            for i in 0..psi[psi_id].len() {
                if i != 0 {
                    print!(",");
                }
                print!(
                    "u_{{{},{}}}",
                    edges[psi[psi_id][i]].0 + 1,
                    edges[psi[psi_id][i]].1 + 1
                );
            }
            println!(r"\}}$");
        }

        println!();
        renumbered.print_node_circle(Self::standard_naming);
        println!();
        for i in 0..self.node_count {
            println!(r"\path (x{}) edge (x{});", i, (i + 1) % self.node_count);
        }
        println!();
        for &i in &psi[maximal_pairs[0].0] {
            edge_used[i] = true;
            println!(r"\path (x{}) edge (x{});", edges[i].0, edges[i].1);
        }
        println!();
        for &i in &psi[maximal_pairs[0].1] {
            edge_used[i] = true;
            let (a, b) = edges[i];
            let mut dist = 0;
            let mut mp1 = 0.0;
            let mut mp2 = 0.0;
            for i in 0..(self.node_count / 2 + 1) {
                if (a + i) % self.node_count == b {
                    dist = i - 1;
                    mp1 = a as f64 + i as f64 * 1.0 / 3.0;
                    mp2 = a as f64 + i as f64 * 2.0 / 3.0;
                    break;
                } else if (b + i) % self.node_count == a {
                    dist = i - 1;
                    mp1 = b as f64 + i as f64 * 2.0 / 3.0;
                    mp2 = b as f64 + i as f64 * 1.0 / 3.0;
                    break;
                }
            }

            let phi1 = 2.0 * std::f64::consts::PI * mp1 / self.node_count as f64;
            let phi2 = 2.0 * std::f64::consts::PI * mp2 / self.node_count as f64;

            println!(
                r"\draw (x{0}) .. controls ({2},{3}) and ({4},{5}) .. (x{1});",
                a,
                b,
                (4.5 + 0.25 * (dist * dist) as f64) * phi1.sin(),
                (4.5 + 0.25 * (dist * dist) as f64) * phi1.cos(),
                (4.5 + 0.25 * (dist * dist) as f64) * phi2.sin(),
                (4.5 + 0.25 * (dist * dist) as f64) * phi2.cos(),
            );
        }

        println!();
        for i in 0..psi.len() {
            let filtered: Vec<_> = psi[i].iter().filter(|&&j| !edge_used[j]).collect();
            if !filtered.is_empty() {
                print!(r"$\psi_{{{}}}=\{{", i + 1);
                for i in 0..filtered.len() {
                    if i != 0 {
                        print!(",");
                    }
                    print!("u_{{{},{}}}", edges[i].0 + 1, edges[i].1 + 1);
                }
                println!(r"\}}$\\");
            }
        }
    }
}

fn main() {
    let variant = Graph::variant();
    //    variant.print_node_circle(Graph::standard_naming);
    //    variant.print_edges();
    variant.gamilton_cycle();
}
