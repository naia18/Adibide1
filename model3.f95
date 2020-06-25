subroutine model3(P_n, I_n, denbora, p1, p2, N, d, M)
! =====================================================
! B Eredurako sortutako subrutina
! Azken aldaketa: Naia Ormaza Zulueta 05/2020
! =====================================================
    integer, intent(in)   :: N, M
    real, intent(in)      :: d
    real, intent(inout)   :: p1, p2
    integer, intent(out)  :: denbora
    real, dimension(0:N-1), intent(out)  :: P_n, I_n
    
    real, dimension(0:1) :: f_i
    integer, dimension(0:1) :: ran_int
    real, parameter :: L=0.01, c=0.01, dp=0.01, dt=0.0001
    integer :: tick
    real :: Delta_PM
    
    f_i(0)=-dp
    f_i(1)=dp
    
    tick = 0
    denbora = 0
    
    Delta_PM = 0.0
    
    P_n(0) = (p1+p2)/2
    I_n(0) = 0
    
    
    do while (tick <= N) 
      do while (abs(p1-p2) <= L)     
         call random_int(ran_int,0,1)
         p1 = p1 + c*f_i(ran_int(0)) + d*Delta_PM*dt
         p2 = p2 + c*f_i(ran_int(1)) + d*Delta_PM*dt
         denbora = denbora + 1  
      end do 
      
      Delta_PM = 0.0             ! Delta_PM hasieratu batez besteko ibiltaria kalkulatzeko
      tick = tick + 1      
      
      avg = (p1+p2)/2
      p1 = avg
      p2 = avg 
      P_n(tick) = avg
      I_n(tick) = denbora
      
      M_s = min(M,tick)
      
      do k=0, M_s-1
        Delta_PM = Delta_PM + (M_s-k)*(P_n(tick-k)-P_n(tick-k-1))
      end do
      
      Delta_PM = Delta_PM*2/(M_s*(M_s+1))
      
    end do 
    
end subroutine


subroutine random_int(random_result, low, high)
!! Returns an integer random value in the range [low, high]. From "Fortran 95 Using F".
     integer, intent(in) :: low, high
     integer, dimension(0:1), intent(out) :: random_result
     real, dimension(0:1) :: random_real
     call init_random_seed() 
     call random_number(random_real)
     random_result = int( (high-low+1)*random_real + low )
     
end subroutine

subroutine init_random_seed()
     integer :: i, n, clock
     integer, dimension(:), allocatable :: seed
          
     call random_seed(size = n)
     allocate(seed(n))
          
     call system_clock(COUNT=clock)
          
     seed = clock + 37 * (/ (i - 1, i = 1, n) /)
     call random_seed(PUT = seed)
          
     deallocate(seed)
end subroutine
