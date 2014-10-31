subroutine f_array_add(p, q, r, a, b, c)

  ! Add conformable arrays a and b to produce result c

  implicit none

  integer, intent(in)  :: p,q,r

  double precision, intent(inout), dimension(r,q,p) :: a
  double precision, intent(inout), dimension(r,q,p) :: b
  double precision, intent(inout), dimension(r,q,p) :: c

  c = a + b

end subroutine f_array_add



subroutine f_matrix_add(m, n, a, b, c)

  ! Add conformable matrices a and b to produce result c

  implicit none

  integer, intent(in)  :: m, n

  double precision, intent(in),    dimension(n,m) :: a
  double precision, intent(in),    dimension(n,m) :: b
  double precision, intent(inout), dimension(n,m) :: c

  c = a + b

end subroutine f_matrix_add



subroutine f_vector_add(n, a, b, c)

  implicit none

  integer, intent(in)  :: n

  double precision, intent(in),    dimension(n) :: a
  double precision, intent(inout), dimension(n) :: b
  double precision, intent(inout), dimension(n) :: c

  c = a + b

end subroutine f_vector_add


!  ----- eof ------------------------------------------------------------------
