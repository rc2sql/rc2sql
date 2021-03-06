(*<*)
theory MFOTL
  imports Interval Trace Abstract_Monitor
begin
(*>*)

section \<open>Metric first-order temporal logic\<close>

context begin

subsection \<open>Formulas and satisfiability\<close>

qualified type_synonym name = string
qualified type_synonym 'a event = "(name \<times> 'a list)"
qualified type_synonym 'a database = "'a event set"
qualified type_synonym 'a prefix = "(name \<times> 'a list) prefix"
qualified type_synonym 'a trace = "(name \<times> 'a list) trace"

qualified type_synonym 'a env = "'a list"

qualified datatype 'a trm = Var nat | is_Const: Const 'a

qualified primrec fvi_trm :: "nat \<Rightarrow> 'a trm \<Rightarrow> nat set" where
  "fvi_trm b (Var x) = (if b \<le> x then {x - b} else {})"
| "fvi_trm b (Const _) = {}"

abbreviation "fv_trm \<equiv> fvi_trm 0"

qualified primrec eval_trm :: "'a env \<Rightarrow> 'a trm \<Rightarrow> 'a" where
  "eval_trm v (Var x) = v ! x"
| "eval_trm v (Const x) = x"

lemma eval_trm_cong: "\<forall>x\<in>fv_trm t. v ! x = v' ! x \<Longrightarrow> eval_trm v t = eval_trm v' t"
  by (cases t) simp_all

qualified datatype (discs_sels) 'a formula = Pred name "'a trm list" | Eq "'a trm" "'a trm"
  | Neg "'a formula" | Or "'a formula" "'a formula" | Exists "'a formula"
  | Prev \<I> "'a formula" | Next \<I> "'a formula"
  | Since "'a formula" \<I> "'a formula" | Until "'a formula" \<I> "'a formula"

qualified primrec fvi :: "nat \<Rightarrow> 'a formula \<Rightarrow> nat set" where
  "fvi b (Pred r ts) = (\<Union>t\<in>set ts. fvi_trm b t)"
| "fvi b (Eq t1 t2) = fvi_trm b t1 \<union> fvi_trm b t2"
| "fvi b (Neg \<phi>) = fvi b \<phi>"
| "fvi b (Or \<phi> \<psi>) = fvi b \<phi> \<union> fvi b \<psi>"
| "fvi b (Exists \<phi>) = fvi (Suc b) \<phi>"
| "fvi b (Prev I \<phi>) = fvi b \<phi>"
| "fvi b (Next I \<phi>) = fvi b \<phi>"
| "fvi b (Since \<phi> I \<psi>) = fvi b \<phi> \<union> fvi b \<psi>"
| "fvi b (Until \<phi> I \<psi>) = fvi b \<phi> \<union> fvi b \<psi>"

abbreviation "fv \<equiv> fvi 0"

lemma finite_fvi_trm[simp]: "finite (fvi_trm b t)"
  by (cases t) simp_all

lemma finite_fvi[simp]: "finite (fvi b \<phi>)"
  by (induction \<phi> arbitrary: b) simp_all

lemma fvi_trm_Suc: "x \<in> fvi_trm (Suc b) t \<longleftrightarrow> Suc x \<in> fvi_trm b t"
  by (cases t) auto

lemma fvi_Suc: "x \<in> fvi (Suc b) \<phi> \<longleftrightarrow> Suc x \<in> fvi b \<phi>"
  by (induction \<phi> arbitrary: b) (simp_all add: fvi_trm_Suc)

lemma fvi_Suc_bound:
  assumes "\<forall>i\<in>fvi (Suc b) \<phi>. i < n"
  shows "\<forall>i\<in>fvi b \<phi>. i < Suc n"
proof
  fix i
  assume "i \<in> fvi b \<phi>"
  with assms show "i < Suc n" by (cases i) (simp_all add: fvi_Suc)
qed

qualified definition nfv :: "'a formula \<Rightarrow> nat" where
  "nfv \<phi> = Max (insert 0 (Suc ` fv \<phi>))"

qualified definition envs :: "'a formula \<Rightarrow> 'a env set" where
  "envs \<phi> = {v. length v = nfv \<phi>}"

lemma nfv_simps[simp]:
  "nfv (Neg \<phi>) = nfv \<phi>"
  "nfv (Or \<phi> \<psi>) = max (nfv \<phi>) (nfv \<psi>)"
  "nfv (Prev I \<phi>) = nfv \<phi>"
  "nfv (Next I \<phi>) = nfv \<phi>"
  "nfv (Since \<phi> I \<psi>) = max (nfv \<phi>) (nfv \<psi>)"
  "nfv (Until \<phi> I \<psi>) = max (nfv \<phi>) (nfv \<psi>)"
  unfolding nfv_def by (simp_all add: image_Un Max_Un[symmetric])

lemma fvi_less_nfv: "\<forall>i\<in>fv \<phi>. i < nfv \<phi>"
  unfolding nfv_def
  by (auto simp add: Max_gr_iff intro: max.strict_coboundedI2)


qualified primrec future_bounded :: "'a formula \<Rightarrow> bool" where
  "future_bounded (Pred _ _) = True"
| "future_bounded (Eq _ _) = True"
| "future_bounded (Neg \<phi>) = future_bounded \<phi>"
| "future_bounded (Or \<phi> \<psi>) = (future_bounded \<phi> \<and> future_bounded \<psi>)"
| "future_bounded (Exists \<phi>) = future_bounded \<phi>"
| "future_bounded (Prev I \<phi>) = future_bounded \<phi>"
| "future_bounded (Next I \<phi>) = future_bounded \<phi>"
| "future_bounded (Since \<phi> I \<psi>) = (future_bounded \<phi> \<and> future_bounded \<psi>)"
| "future_bounded (Until \<phi> I \<psi>) = (future_bounded \<phi> \<and> future_bounded \<psi> \<and> bounded I)"


qualified primrec sat :: "'a trace \<Rightarrow> 'a env \<Rightarrow> nat \<Rightarrow> 'a formula \<Rightarrow> bool" where
  "sat \<sigma> v i (Pred r ts) = ((r, map (eval_trm v) ts) \<in> \<Gamma> \<sigma> i)"
| "sat \<sigma> v i (Eq t1 t2) = (eval_trm v t1 = eval_trm v t2)"
| "sat \<sigma> v i (Neg \<phi>) = (\<not> sat \<sigma> v i \<phi>)"
| "sat \<sigma> v i (Or \<phi> \<psi>) = (sat \<sigma> v i \<phi> \<or> sat \<sigma> v i \<psi>)"
| "sat \<sigma> v i (Exists \<phi>) = (\<exists>z. sat \<sigma> (z # v) i \<phi>)"
| "sat \<sigma> v i (Prev I \<phi>) = (case i of 0 \<Rightarrow> False | Suc j \<Rightarrow> mem I (\<tau> \<sigma> i - \<tau> \<sigma> j) \<and> sat \<sigma> v j \<phi>)"
| "sat \<sigma> v i (Next I \<phi>) = (mem I (\<tau> \<sigma> (Suc i) - \<tau> \<sigma> i) \<and> sat \<sigma> v (Suc i) \<phi>)"
| "sat \<sigma> v i (Since \<phi> I \<psi>) = (\<exists>j\<le>i. mem I (\<tau> \<sigma> i - \<tau> \<sigma> j) \<and> sat \<sigma> v j \<psi> \<and> (\<forall>k \<in> {j <.. i}. sat \<sigma> v k \<phi>))"
| "sat \<sigma> v i (Until \<phi> I \<psi>) = (\<exists>j\<ge>i. mem I (\<tau> \<sigma> j - \<tau> \<sigma> i) \<and> sat \<sigma> v j \<psi> \<and> (\<forall>k \<in> {i ..< j}. sat \<sigma> v k \<phi>))"

lemma sat_Until_rec: "sat \<sigma> v i (Until \<phi> I \<psi>) \<longleftrightarrow>
  (memL I 0 \<and> sat \<sigma> v i \<psi> \<or>
  memR I (\<Delta> \<sigma> (i + 1)) \<and> sat \<sigma> v i \<phi> \<and> sat \<sigma> v (i + 1) (Until \<phi> (subtract (\<Delta> \<sigma> (i + 1)) I) \<psi>))"
  (is "?L \<longleftrightarrow> ?R")
proof (rule iffI; (elim disjE conjE)?)
  assume ?L
  then obtain j where j: "i \<le> j" "mem I (\<tau> \<sigma> j - \<tau> \<sigma> i)" "sat \<sigma> v j \<psi>" "\<forall>k \<in> {i ..< j}. sat \<sigma> v k \<phi>"
    by auto
  then show ?R
  proof (cases "i = j")
    case False
    with j(1,2) have "memR I (\<Delta> \<sigma> (i + 1))"
      by (auto simp: diff_le_mono)
    moreover from False j(1,4) have "sat \<sigma> v i \<phi>" by auto
    moreover from False j have "sat \<sigma> v (i + 1) (Until \<phi> (subtract (\<Delta> \<sigma> (i + 1)) I) \<psi>)"
      by (auto intro!: exI[of _ j])
    ultimately show ?thesis by blast
  qed simp
next
  assume \<Delta>: "memR I (\<Delta> \<sigma> (i + 1))" and now: "sat \<sigma> v i \<phi>" and
   "next": "sat \<sigma> v (i + 1) (Until \<phi> (subtract (\<Delta> \<sigma> (i + 1)) I) \<psi>)"
  from "next" obtain j where j: "i + 1 \<le> j" "mem ((subtract (\<Delta> \<sigma> (i + 1)) I)) (\<tau> \<sigma> j - \<tau> \<sigma> (i + 1))"
      "sat \<sigma> v j \<psi>" "\<forall>k \<in> {i + 1 ..< j}. sat \<sigma> v k \<phi>"
    by (auto simp: diff_le_mono memL_mono)
  from \<Delta> j(1,2) have "mem I (\<tau> \<sigma> j - \<tau> \<sigma> i)"
    by auto
  with now j(1,3,4) show ?L by (auto simp: le_eq_less_or_eq[of i] intro!: exI[of _ j])
qed auto

lemma sat_Since_rec: "sat \<sigma> v i (Since \<phi> I \<psi>) \<longleftrightarrow>
  memL I 0 \<and> sat \<sigma> v i \<psi> \<or>
  (i > 0 \<and> memR I (\<Delta> \<sigma> i) \<and> sat \<sigma> v i \<phi> \<and> sat \<sigma> v (i - 1) (Since \<phi> (subtract (\<Delta> \<sigma> i) I) \<psi>))"
  (is "?L \<longleftrightarrow> ?R")
proof (rule iffI; (elim disjE conjE)?)
  assume ?L
  then obtain j where j: "j \<le> i" "mem I (\<tau> \<sigma> i - \<tau> \<sigma> j)" "sat \<sigma> v j \<psi>" "\<forall>k \<in> {j <.. i}. sat \<sigma> v k \<phi>"
    by auto
  then show ?R
  proof (cases "i = j")
    case False
    with j(1) obtain k where [simp]: "i = k + 1"
      by (cases i) auto
    with j(1,2) False have "memR I (\<Delta> \<sigma> i)"
      by (auto simp: diff_le_mono2)
    moreover from False j(1,4) have "sat \<sigma> v i \<phi>" by auto
    moreover from False j have "sat \<sigma> v (i - 1) (Since \<phi> (subtract (\<Delta> \<sigma> i) I) \<psi>)"
      by (auto intro!: exI[of _ j])
    ultimately show ?thesis by auto
  qed simp
next
  assume i: "0 < i" and \<Delta>: "memR I (\<Delta> \<sigma> i)" and now: "sat \<sigma> v i \<phi>" and
   "prev": "sat \<sigma> v (i - 1) (Since \<phi> (subtract (\<Delta> \<sigma> i) I) \<psi>)"
  from "prev" obtain j where j: "j \<le> i - 1" "mem ((subtract (\<Delta> \<sigma> i) I)) (\<tau> \<sigma> (i - 1) - \<tau> \<sigma> j)"
      "sat \<sigma> v j \<psi>" "\<forall>k \<in> {j <.. i - 1}. sat \<sigma> v k \<phi>"
    by (auto simp: diff_le_mono2 memL_mono)
  from \<Delta> i j(1,2) have "mem I (\<tau> \<sigma> i - \<tau> \<sigma> j)"
    by auto
  with now i j(1,3,4) show ?L by (auto simp: le_Suc_eq gr0_conv_Suc intro!: exI[of _ j])
qed auto

lemma sat_Since_0: "sat \<sigma> v 0 (Since \<phi> I \<psi>) \<longleftrightarrow> memL I 0 \<and> sat \<sigma> v 0 \<psi>"
  by auto

lemma sat_Since_point: "sat \<sigma> v i (Since \<phi> I \<psi>) \<Longrightarrow>
    (\<And>j. j \<le> i \<Longrightarrow> mem I (\<tau> \<sigma> i - \<tau> \<sigma> j) \<Longrightarrow> sat \<sigma> v i (Since \<phi> (point (\<tau> \<sigma> i - \<tau> \<sigma> j)) \<psi>) \<Longrightarrow> P) \<Longrightarrow> P"
  by (auto intro: diff_le_self)

lemma sat_Since_pointD: "sat \<sigma> v i (Since \<phi> (point t) \<psi>) \<Longrightarrow> mem I t \<Longrightarrow> sat \<sigma> v i (Since \<phi> I \<psi>)"
  by auto

lemma eval_trm_fvi_cong: "\<forall>x\<in>fv_trm t. v!x = v'!x \<Longrightarrow> eval_trm v t = eval_trm v' t"
  by (cases t) simp_all

lemma sat_fvi_cong: "\<forall>x\<in>fv \<phi>. v!x = v'!x \<Longrightarrow> sat \<sigma> v i \<phi> = sat \<sigma> v' i \<phi>"
proof (induct \<phi> arbitrary: v v' i)
  case (Pred n ts)
  show ?case by (simp cong: map_cong eval_trm_fvi_cong[OF Pred[simplified, THEN bspec]])
next
  case (Eq x1 x2)
  then show ?case  unfolding fvi.simps sat.simps by (metis UnCI eval_trm_fvi_cong)
next
  case (Exists \<phi>)
  then show ?case unfolding sat.simps by (intro iff_exI) (simp add: fvi_Suc nth_Cons')
qed (auto 8 0 simp add: nth_Cons' split: nat.splits intro!: iff_exI)


subsection \<open>Defined connectives\<close>

qualified definition "And \<phi> \<psi> = Neg (Or (Neg \<phi>) (Neg \<psi>))"

lemma fvi_And: "fvi b (And \<phi> \<psi>) = fvi b \<phi> \<union> fvi b \<psi>"
  unfolding And_def by simp

lemma nfv_And[simp]: "nfv (And \<phi> \<psi>) = max (nfv \<phi>) (nfv \<psi>)"
  unfolding nfv_def by (simp add: fvi_And image_Un Max_Un[symmetric])

lemma future_reach_And: "future_bounded (And \<phi> \<psi>) = (future_bounded \<phi> \<and> future_bounded \<psi>)"
  unfolding And_def by simp

lemma sat_And: "sat \<sigma> v i (And \<phi> \<psi>) = (sat \<sigma> v i \<phi> \<and> sat \<sigma> v i \<psi>)"
  unfolding And_def by simp

qualified definition "And_Not \<phi> \<psi> = Neg (Or (Neg \<phi>) \<psi>)"

lemma fvi_And_Not: "fvi b (And_Not \<phi> \<psi>) = fvi b \<phi> \<union> fvi b \<psi>"
  unfolding And_Not_def by simp

lemma nfv_And_Not[simp]: "nfv (And_Not \<phi> \<psi>) = max (nfv \<phi>) (nfv \<psi>)"
  unfolding nfv_def by (simp add: fvi_And_Not image_Un Max_Un[symmetric])

lemma future_reach_And_Not: "future_bounded (And_Not \<phi> \<psi>) = (future_bounded \<phi> \<and> future_bounded \<psi>)"
  unfolding And_Not_def by simp

lemma sat_And_Not: "sat \<sigma> v i (And_Not \<phi> \<psi>) = (sat \<sigma> v i \<phi> \<and> \<not> sat \<sigma> v i \<psi>)"
  unfolding And_Not_def by simp


subsection \<open>Safe formulas\<close>

fun safe_formula :: "'a MFOTL.formula \<Rightarrow> bool" where
  "safe_formula (MFOTL.Eq t1 t2) = (MFOTL.is_Const t1 \<or> MFOTL.is_Const t2)"
| "safe_formula (MFOTL.Neg (MFOTL.Eq (MFOTL.Const x) (MFOTL.Const y))) = True"
| "safe_formula (MFOTL.Neg (MFOTL.Eq (MFOTL.Var x) (MFOTL.Var y))) = (x = y)"
| "safe_formula (MFOTL.Pred e ts) = True"
| "safe_formula (MFOTL.Neg (MFOTL.Or (MFOTL.Neg \<phi>) \<psi>)) = (safe_formula \<phi> \<and>
    (safe_formula \<psi> \<and> MFOTL.fv \<psi> \<subseteq> MFOTL.fv \<phi> \<or> (case \<psi> of MFOTL.Neg \<psi>' \<Rightarrow> safe_formula \<psi>' | _ \<Rightarrow> False)))"
| "safe_formula (MFOTL.Or \<phi> \<psi>) = (MFOTL.fv \<psi> = MFOTL.fv \<phi> \<and> safe_formula \<phi> \<and> safe_formula \<psi>)"
| "safe_formula (MFOTL.Exists \<phi>) = (safe_formula \<phi>)"
| "safe_formula (MFOTL.Prev I \<phi>) = (safe_formula \<phi>)"
| "safe_formula (MFOTL.Next I \<phi>) = (safe_formula \<phi>)"
| "safe_formula (MFOTL.Since \<phi> I \<psi>) = (MFOTL.fv \<phi> \<subseteq> MFOTL.fv \<psi> \<and>
    (safe_formula \<phi> \<or> (case \<phi> of MFOTL.Neg \<phi>' \<Rightarrow> safe_formula \<phi>' | _ \<Rightarrow> False)) \<and> safe_formula \<psi>)"
| "safe_formula (MFOTL.Until \<phi> I \<psi>) = (MFOTL.fv \<phi> \<subseteq> MFOTL.fv \<psi> \<and>
    (safe_formula \<phi> \<or> (case \<phi> of MFOTL.Neg \<phi>' \<Rightarrow> safe_formula \<phi>' | _ \<Rightarrow> False)) \<and> safe_formula \<psi>)"
| "safe_formula _ = False"

lemma disjE_Not2: "P \<or> Q \<Longrightarrow> (P \<Longrightarrow> R) \<Longrightarrow> (\<not>P \<Longrightarrow> Q \<Longrightarrow> R) \<Longrightarrow> R"
  by blast

lemma safe_formula_induct[consumes 1]:
  assumes "safe_formula \<phi>"
    and "\<And>t1 t2. MFOTL.is_Const t1 \<Longrightarrow> P (MFOTL.Eq t1 t2)"
    and "\<And>t1 t2. MFOTL.is_Const t2 \<Longrightarrow> P (MFOTL.Eq t1 t2)"
    and "\<And>x y. P (MFOTL.Neg (MFOTL.Eq (MFOTL.Const x) (MFOTL.Const y)))"
    and "\<And>x y. x = y \<Longrightarrow> P (MFOTL.Neg (MFOTL.Eq (MFOTL.Var x) (MFOTL.Var y)))"
    and "\<And>e ts. P (MFOTL.Pred e ts)"
    and "\<And>\<phi> \<psi>. \<not> (safe_formula (MFOTL.Neg \<psi>) \<and> MFOTL.fv \<psi> \<subseteq> MFOTL.fv \<phi>) \<Longrightarrow> P \<phi> \<Longrightarrow> P \<psi> \<Longrightarrow> P (MFOTL.And \<phi> \<psi>)"
    and "\<And>\<phi> \<psi>. safe_formula \<psi> \<Longrightarrow> MFOTL.fv \<psi> \<subseteq> MFOTL.fv \<phi> \<Longrightarrow> P \<phi> \<Longrightarrow> P \<psi> \<Longrightarrow> P (MFOTL.And_Not \<phi> \<psi>)"
    and "\<And>\<phi> \<psi>. MFOTL.fv \<psi> = MFOTL.fv \<phi> \<Longrightarrow> P \<phi> \<Longrightarrow> P \<psi> \<Longrightarrow> P (MFOTL.Or \<phi> \<psi>)"
    and "\<And>\<phi>. P \<phi> \<Longrightarrow> P (MFOTL.Exists \<phi>)"
    and "\<And>I \<phi>. P \<phi> \<Longrightarrow> P (MFOTL.Prev I \<phi>)"
    and "\<And>I \<phi>. P \<phi> \<Longrightarrow> P (MFOTL.Next I \<phi>)"
    and "\<And>\<phi> I \<psi>. MFOTL.fv \<phi> \<subseteq> MFOTL.fv \<psi> \<Longrightarrow> safe_formula \<phi> \<Longrightarrow> P \<phi> \<Longrightarrow> P \<psi> \<Longrightarrow> P (MFOTL.Since \<phi> I \<psi>)"
    and "\<And>\<phi> I \<psi>. MFOTL.fv (MFOTL.Neg \<phi>) \<subseteq> MFOTL.fv \<psi> \<Longrightarrow>
      \<not> safe_formula (MFOTL.Neg \<phi>) \<Longrightarrow> P \<phi> \<Longrightarrow> P \<psi> \<Longrightarrow> P (MFOTL.Since (MFOTL.Neg \<phi>) I \<psi> )"
    and "\<And>\<phi> I \<psi>. MFOTL.fv \<phi> \<subseteq> MFOTL.fv \<psi> \<Longrightarrow> safe_formula \<phi> \<Longrightarrow> P \<phi> \<Longrightarrow> P \<psi> \<Longrightarrow> P (MFOTL.Until \<phi> I \<psi>)"
    and "\<And>\<phi> I \<psi>. MFOTL.fv (MFOTL.Neg \<phi>) \<subseteq> MFOTL.fv \<psi> \<Longrightarrow>
      \<not> safe_formula (MFOTL.Neg \<phi>) \<Longrightarrow> P \<phi> \<Longrightarrow> P \<psi> \<Longrightarrow> P (MFOTL.Until (MFOTL.Neg \<phi>) I \<psi>)"
  shows "P \<phi>"
  using assms(1)
proof (induction rule: safe_formula.induct)
  case (5 \<phi> \<psi>)
  then show ?case
    by (cases \<psi>)
      (auto 0 3 elim!: disjE_Not2 intro: assms[unfolded MFOTL.And_def MFOTL.And_Not_def])
next
  case (10 \<phi> I \<psi>)
  then show ?case
    by (cases \<phi>) (auto 0 3 elim!: disjE_Not2 intro: assms)
next
  case (11 \<phi> I \<psi>)
  then show ?case
    by (cases \<phi>) (auto 0 3 elim!: disjE_Not2 intro: assms)
qed (auto intro: assms)


subsection \<open>Slicing traces\<close>

qualified primrec matches :: "'a env \<Rightarrow> 'a formula \<Rightarrow> name \<times> 'a list \<Rightarrow> bool" where
  "matches v (Pred r ts) e = (r = fst e \<and> map (eval_trm v) ts = snd e)"
| "matches v (Eq _ _) e = False"
| "matches v (Neg \<phi>) e = matches v \<phi> e"
| "matches v (Or \<phi> \<psi>) e = (matches v \<phi> e \<or> matches v \<psi> e)"
| "matches v (Exists \<phi>) e = (\<exists>z. matches (z # v) \<phi> e)"
| "matches v (Prev I \<phi>) e = matches v \<phi> e"
| "matches v (Next I \<phi>) e = matches v \<phi> e"
| "matches v (Since \<phi> I \<psi>) e = (matches v \<phi> e \<or> matches v \<psi> e)"
| "matches v (Until \<phi> I \<psi>) e = (matches v \<phi> e \<or> matches v \<psi> e)"

lemma matches_fvi_cong: "\<forall>x\<in>fv \<phi>. v!x = v'!x \<Longrightarrow> matches v \<phi> e = matches v' \<phi> e"
proof (induct \<phi> arbitrary: v v')
  case (Pred n ts)
  show ?case by (simp cong: map_cong eval_trm_fvi_cong[OF Pred[simplified, THEN bspec]])
next
  case (Exists \<phi>)
  then show ?case unfolding matches.simps by (intro iff_exI) (simp add: fvi_Suc nth_Cons')
qed (auto 5 0 simp add: nth_Cons')

abbreviation relevant_events where "relevant_events \<phi> S \<equiv> {e. S \<inter> {v. matches v \<phi> e} \<noteq> {}}"

lemma sat_slice_strong: "relevant_events \<phi> S \<subseteq> E \<Longrightarrow> v \<in> S \<Longrightarrow>
  sat \<sigma> v i \<phi> \<longleftrightarrow> sat (map_\<Gamma> (\<lambda>D. D \<inter> E) \<sigma>) v i \<phi>"
proof (induction \<phi> arbitrary: v S i)
  case (Pred r ts)
  then show ?case by (auto simp: subset_eq)
next
  case (Eq t1 t2)
  show ?case
    unfolding sat.simps
    by simp
next
  case (Neg \<phi>)
  then show ?case by simp
next
  case (Or \<phi> \<psi>)
  show ?case using Or.IH[of S] Or.prems
    by (auto simp: Collect_disj_eq Int_Un_distrib subset_iff)
next
  case (Exists \<phi>)
  have "sat \<sigma> (z # v) i \<phi> = sat (map_\<Gamma> (\<lambda>D. D \<inter> E) \<sigma>) (z # v) i \<phi>" for z
    using Exists.prems by (auto intro!: Exists.IH[of "{z # v | v. v \<in> S}"])
  then show ?case by simp
next
  case (Prev I \<phi>)
  then show ?case by (auto cong: nat.case_cong)
next
  case (Next I \<phi>)
  then show ?case by simp
next
  case (Since \<phi> I \<psi>)
  show ?case using Since.IH[of S] Since.prems
   by (auto simp: Collect_disj_eq Int_Un_distrib subset_iff)
next
  case (Until \<phi> I \<psi>)
  show ?case using Until.IH[of S] Until.prems
   by (auto simp: Collect_disj_eq Int_Un_distrib subset_iff)
qed

end (*context*)

interpretation MFOTL_slicer: abstract_slicer "relevant_events \<phi>" for \<phi> .

lemma sat_slice_iff:
  assumes "v \<in> S"
  shows "MFOTL.sat \<sigma> v i \<phi> \<longleftrightarrow> MFOTL.sat (MFOTL_slicer.slice \<phi> S \<sigma>) v i \<phi>"
  by (rule sat_slice_strong[of S, OF subset_refl assms])

lemma slice_replace_prefix:
  "prefix_of (MFOTL_slicer.pslice \<phi> R \<pi>) \<sigma> \<Longrightarrow>
    MFOTL_slicer.slice \<phi> R (replace_prefix \<pi> \<sigma>) = MFOTL_slicer.slice \<phi> R \<sigma>"
  by (rule map_\<Gamma>_replace_prefix) auto

(*<*)
end
(*>*)