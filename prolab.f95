program prolab
implicit none
real(kind=8)::x
real(kind=8)::sum_, sum_2 = 0
real(kind=8)::n = 0
real(kind=8)::m, s
real(kind=8)::k = 1
do
   print*,"Inserisci il valore",int(k)
   k = k + 1
   read(*,*)x
   if (x > 0) then
      sum_ = sum_ + x
      sum_2 = sum_2 + (x**2)
      n = n + 1.
   end if
   if (x == 0) exit

end do
m = (sum_)/n
s = sqrt((n*sum_2-sum_**2)/(n*(n-1)))
print*,"media =",m
print*,"deviazione standard =",s
print*,"    "
print*,"Classi con intervalli uguali a Sx e simmetrici rispetto a m"
print*,"Classe 1 da ", m - 4*s," a ", m -3*s
print*,"Classe 2 da ", m - 3*s," a ", m -2*s
print*,"Classe 3 da ", m - 2*s," a ", m -1*s
print*,"Classe 4 da ", m - s," a ", m
print*,"Classe 5 da ", m ," a ", m + s
print*,"Classe 6 da ", m + s," a ", m + 2*s
print*,"Classe 7 da ", m + 2*s," a ", m + 3*s
print*,"Classe 8 da ", m + 3*s," a ", m + 4*s
print*,"    "
print*,"Classi con intervalli uguali a Sx/2 con m punto medio"
print*,"Classe 1 da ", m - (7*s)/2," a ", m - (5*s)/2
print*,"Classe 2 da ", m - (5*s)/2," a ", m - (3*s)/2
print*,"Classe 3 da ", m - (3*s)/2," a ", m - s/2
print*,"Classe 4 da ", m - s/2," a ", m + s/2
print*,"Classe 5 da ", m + s/2," a ", m + (3*s)/2
print*,"Classe 6 da ", m + (3*s)/2," a ", m + (5*s)/2
print*,"Classe 7 da ", m + (5*s)/2," a ", m + (7*s)/2
print*,"deviazione standard della media =",s/(sqrt(n))
end prolab
