       PROGRAM PARAMCONVERSION
       IMPLICIT NONE
       REAL*8 X0,Y0,Z0,X0DOT,Y0DOT,Z0DOT,ANO,XRES,RESDOT,X(3,1)
       REAL*8 XDOT(3,1)
       WRITE(6,*)'X,Y,Z,XDOT,YDOT,ZDOT,T'
       READ(5,*)X0,Y0,Z0,X0DOT,Y0DOT,Z0DOT,ANO
       X(1,1)=X0
       X(2,1)=Y0
       X(3,1)=Z0
       XDOT(1,1)=X0DOT
       XDOT(2,1)=Y0DOT
       XDOT(3,1)=Z0DOT
       CALL SUBROUTINE ITRF2014to2008(X,XDOT,ANO,XRES,RESDOT)     
       END


c      Parametros ITRF 2014 para ITRF 2008
c      Epoca 2010
       SUBROUTINE ITRF2014to2008(X,XDOT,ANO,XRES,RESDOT)  
       IMPLICIT NONE
       REAL*8 EPOCA,X(3,1),XDOT(3,1),ANO,R(3,3),RDOT(3,3),T(3,1)
       REAL*8 TDOT(3,1),XRES(3,1),RESDOT,DT,PI,T1,T2,T3,D,R1,R2
       REAL*8 R3,T1DOT,T2DOT,T3DOT,DDOT,R1DOT,R2DOT,R3DOT,CORT1,CORT2
       REAL*8 CORT3,CORD,CORR1,CORR2,CORR3,AUX1,AUX2
       REAL*8 AUX3,AUX4,AUX1DOT,AUX2DOT,AUX3DOT,AUX4DOT
       
       
       EPOCA=2010.0D0
       DT=ANO-EPOCA
       PI=4.0D0*DATAN(1.0D0)
C      PARAMETROS DE TRANSFORMACAO
       T1=1.6D-3
       T2=1.9D-3
       T3=2.4D-3
       D=-0.02D-9
       R1=0.0D-3*PI/(180.0D0*3600.0D0)
       R2=0.0D-3*PI/(180.0D0*3600.0D0)
       R3=0.0D-3*PI/(180.0D0*3600.0D0)
       T1DOT=0.0D-3
       T2DOT=0.0D-3
       T3DOT=-0.1D-3
       DDOT=0.03D-9
       R1DOT=0.0D-3*PI/(180.0D0*3600.0D0)
       R2DOT=0.0D-3*PI/(180.0D0*3600.0D0)
       R3DOT=0.0D-3*PI/(180.0D0*3600.0D0)
C      CORRECAO AOS PARAMETROS DE TRANSFORMACAO 
       CORT1=T1+DT*T1DOT
       CORT2=T2+DT*T2DOT
       CORT3=T3+DT*T3DOT
       CORD=D+DT*DDOT
       CORR1=R1+DT*R1DOT
       CORR2=R2+DT*R2DOT
       CORR3=R3+DT*R3DOT
C      MATRIZES DE TRANSFORMACAO
       R(1,1)= 0
       R(2,1)= -CORR3
       R(3,1)= CORR2
       R(1,2)= CORR3
       R(2,2)= 0
       R(3,2)=-CORR1 
       R(1,3)=-CORR2
       R(2,3)= CORR1
       R(3,3)= 0

       RDOT(1,1)= 0
       RDOT(2,1)= -R3DOT
       RDOT(3,1)= R2DOT
       RDOT(1,2)= R3DOT
       RDOT(2,2)= 0
       RDOT(3,2)=-R1DOT 
       RDOT(1,3)=-R2DOT
       RDOT(2,3)= R1DOT
       RDOT(3,3)= 0
       
       T(1,1)=CORT1
       T(2,1)=CORT2
       T(3,1)=CORT3
       TDOT(1,1)=T1DOT
       TDOT(2,1)=T2DOT
       TDOT(3,1)=T3DOT 



C      SUBROUTINE MATSCA(A,B,C,NM,NN,M,N)

C      CALCULO DE RESULTADOS
c      XRES=X+T+D*X+R*X
       CALL AB (R,X,AUX1,3,3,1,3,3,1)
       CALL MATSCA (D,X,AUX2,3,1,3,1)
       CALL ADDAB(AUX1,AUX2,AUX3,3,1,3,1)
       CALL ADDAB(AUX3,T,AUX4,3,1,3,1)
       CALL ADDAB(AUX4,X,XRES,3,1,3,1)
c      RESDOT=XDOT+TDOT+DDOT*X+RDOT*X       
       CALL AB (RDOT,X,AUX1DOT,3,3,1,3,3,1)
       CALL MATSCA (DDOT,X,AUX2DOT,3,1,3,1)
       CALL ADDAB(AUX1DOT,AUX2DOT,AUX3DOT,3,1,3,1)
       CALL ADDAB(AUX3DOT,TDOT,AUX4DOT,3,1,3,1)
       CALL ADDAB(AUX4DOT,XDOT,RESDOT,3,1,3,1)
       END 
              
C   SUBROTINA PARA EFECTUAR O PRODUTO DAS MATRIZES A(L,M)
C   E B(M,N), SENDO O RESULTADO COLOCADO NA MATRIZ R(L,N).
C   AS DIMENSOES MAXIMAS DAS MATRIZES (NL,NM,NN) 
C   BEM COMO AS EFECTIVAS (L,M,N), SAO PASSADAS DO PROGRAMA PRINCIPAL.
C   AS MATRIZES A E B PODEM SER IGUAIS. R TEM DE SER DIFERENTE
C   DE A E DE B, AS QUAIS VOLTAM INALTERADAS AO PROGRAMA PRINCIPAL.

      SUBROUTINE AB(A,B,R,NL,NM,NN,L,M,N)
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 A(NL,NM),B(NM,NN),R(NL,NN)
      DO 5 I=1,L
      DO 5 J=1,N
      R(I,J)=0
      DO 5 K=1,M
    5 R(I,J)=R(I,J)+A(I,K)*B(K,J)
      RETURN
      END
      
      
C   SUBROTINA PARA FORMAR A MATRIZ SOMA R=A+B.                          
C   AS MATRIZES A,B E R PODEM SER IGUAIS.                               
C                                                                       
C   AS DIMENSOES MAXIMAS DAS MATRIZES (NL,NM),          
C   BEM COMO AS EFECTIVAS (L,M), SAO PASSADAS DO 
C   PROGRAMA PRINCIPAL.          
C                                                                       
      SUBROUTINE ADDAB(A,B,R,NL,NM,L,M)                                 
      IMPLICIT REAL*8(A-H,O-Z)                                          
      REAL*8 A(NL,NM),B(NL,NM),R(NL,NM)                              
      DO 5 J=1,M                                                        
      DO 5 I=1,L                                                        
    5 R(I,J)=A(I,J)+B(I,J)                                              
      RETURN                                                            
      END   
      
C   SUBROTINA PARA MULTIPLICAR A MATRIZ B PELO ESCALAR A                
C   E COLOCA O RESULTADO EM C.                                          
C                                                                       
C   AS MATRIZES B E C PODEM SER IGUAIS.                                 
C                                                                       
C   AS DIMENSOES MAXIMAS DAS MATRIZES (NM,NN),           
C   BEM COMO AS EFECTIVAS (M,N), SAO PASSADAS DO PROGRAMA PRINCIPAL.          
C                                                                       
      SUBROUTINE MATSCA(A,B,C,NM,NN,M,N)                                
      IMPLICIT REAL*8(A-C)                                              
      REAL*8 B(NM,NN),C(NM,NN)                                       
      DO 5 I=1,M                                                        
      DO 5 J=1,N                                                        
    5 C(I,J) = A * B(I,J)                                               
      RETURN                                                            
      END                                                               
