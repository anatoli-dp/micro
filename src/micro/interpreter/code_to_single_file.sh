 # Clear the destination file first to avoid appending on re-runs
> combined_code.txt

# Find files and loop through them
find . \( -name "*.c" -o -name "*.h" \) -print0 | while IFS= read -r -d '' file; do
  echo "==================================================" >> combined_code.txt
  echo "FILE: $file" >> combined_code.txt
  echo "==================================================" >> combined_code.txt
  cat "$file" >> combined_code.txt
  echo -e "\n\n" >> combined_code.txt
done
