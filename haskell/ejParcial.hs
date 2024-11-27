data Buffer a = Empty | Write Int a (Buffer a )| Read Int (Int -> Buffer a)
