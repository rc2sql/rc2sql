python3 init.py ${1} ${2}

for((i=${3}; i<=5; i++))
do
  echo "Query $i"
  cat q${i}.sql
  psql < init.sql > /dev/null
  psql < q${i}.sql | grep -v "rows" | grep -A666 "Planning Time"
  echo ""
done
