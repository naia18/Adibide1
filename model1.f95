subroutine model1(P_n, I_n, denbora, p1, p2, N)
! =====================================================
! This applies the Dealer model 1 for loop
! till all the N = 5000 transactions are achieved.
! A subroutine is used preferred to a function because
! two outputs are attended.
! =====================================================
    integer, intent(in)  :: N
    real, intent(inout)   :: p1, p2
    integer, intent(out) :: denbora
    real, dimension(0:N-1), intent(out)  :: P_n, I_n
    
    real, dimension(0:1) :: f_i
    real, parameter :: L=0.01, c=0.01, dp=0.01
    integer :: tick
    real :: ran_real
    
    f_i(0)=-dp
    f_i(1)=dp
    
    tick = 0
    denbora = 0
    
    do while (tick <= N) 
      do while (abs(p1-p2) <= L)     
         !call random_int(ran_real,1,2)
         call random_seed()
         call random_number(ran_real)
         p1 = p1 + c*f_i(int(2*ran_real))
         call random_number(ran_real)
         p2 = p2 + c*f_i(int(2*ran_real))
         denbora = denbora + 1  
      end do 
      
      avg = (p1+p2)/2
      p1 = avg
      p2 = avg 
      P_n(tick) = avg
      I_n(tick) = denbora
      tick = tick + 1
    end do 
    
end subroutine


!subroutine random_int(random_result, low, high)
!! Returns an integer random value in the range [low, high]. From "Fortran 95 Using F".
!     integer, intent(in) :: low, high
!     integer, intent(out) :: random_result
!     real :: random_real
!     call random_number(random_real)
!     random_result = int( (high-low+1)*random_real + low )
!     
!end subroutine
