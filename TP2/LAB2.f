      PROGRAM LAB1
      IMPLICIT NONE
      REAL*8 X_14(3,1),XDOT_14(3,1),X_08(3,1),XDOT_08(3,1),EPOCA
      REAL*8 X_05(3,1),XDOT_05(3,1),X_00(3,1),XDOT_00(3,1)
      REAL*8 X_E00(3,1),XDOT_E00(3,1)
c      INTEGER 
c     CHARACTER
      WRITE(6,*)'LAB 2'
      X_14(1,1)=4917536.9083D0
      X_14(2,1)=-815726.1130D0
      X_14(3,1)=3965857.4508D0
      XDOT_14(1,1)=-0.00783D0
      XDOT_14(2,1)=0.01940D0
      XDOT_14(3,1)=0.01302D0
      EPOCA=2010.0D0
      
      CALL I14_I08(X_14,XDOT_14,EPOCA,X_08,XDOT_08)
      WRITE(6,*) 'ITRS2014-ITRS2008 '
      WRITE(6,*) 'Coordenadas: ',X_08
      WRITE(6,*) 'Velocidades: ',XDOT_08
      CALL I08_I05(X_08,XDOT_08,EPOCA,X_05,XDOT_05)
      WRITE(6,*) 'ITRS2008-ITRS2005 '
      WRITE(6,*) 'Coordenadas: ',X_05
      WRITE(6,*) 'Velocidades: ',XDOT_05
      CALL I05_I00(X_05,XDOT_05,EPOCA,X_00,XDOT_00)
      WRITE(6,*) 'ITRS2005-ITRS2000 '
      WRITE(6,*) 'Coordenadas: ',X_00
      WRITE(6,*) 'Velocidades: ',XDOT_00
      CALL I00_E00(X_00,XDOT_00,EPOCA,X_E00,XDOT_E00)
      WRITE(6,*) 'ITRS2000-ETRS2000 '
      WRITE(6,*) 'Coordenadas: ',X_E00
      WRITE(6,*) 'Velocidades: ',XDOT_E00
      
      END
      
      
      SUBROUTINE I14_I08(X_14,XDOT_14,EPOCA,X_08,XDOT_08)                            
      IMPLICIT NONE
      REAL*8 X_14(3,1),XDOT_14(3,1),X_08(3,1),XDOT_08(3,1),EPOCA
      REAL*8 T(3,1),D,R(3,3),TDOT(3,1),DDOT,RDOT(3,3),DIF_EPOCA
      REAL*8 A1(3,1),A2(3,1),A3(3,1),A4(3,1),FATOR,PI
      REAL*8 A1DOT(3,1),A2DOT(3,1),A3DOT(3,1),A4DOT(3,1) 
      PI= 4.D0*DATAN(1.D0)
      FATOR=(1.0D-3/3600)*(PI/180)
      
C     RATES - PARAMETERS
      TDOT(1,1)=0.0D-3
      TDOT(2,1)=0.0D-3
      TDOT(3,1)=-0.1D-3
      DDOT=0.03D-9
      RDOT(1,1)=0.0D0*FATOR
      RDOT(1,2)=0.0D0*FATOR
      RDOT(1,3)=0.0D0*FATOR
      RDOT(2,1)=0.0D0*FATOR
      RDOT(2,2)=0.0D0*FATOR
      RDOT(2,3)=0.0D0*FATOR
      RDOT(3,1)=0.0D0*FATOR
      RDOT(3,2)=0.0D0*FATOR
      RDOT(3,3)=0.0D0*FATOR
c     DIFERENCAS DE EPOCAS
      DIF_EPOCA=EPOCA-2010.0D0
c     CALCULO DOS PARAMETROS ATUALIZADOS      
      T(1,1)=1.6D-3+DIF_EPOCA*TDOT(1,1)
      T(2,1)=1.9D-3+DIF_EPOCA*TDOT(2,1)
      T(3,1)=2.4D-3+DIF_EPOCA*TDOT(3,1)
      D=-0.02D-9+DIF_EPOCA*DDOT   
      R(1,1)=0.0D0*FATOR+DIF_EPOCA*RDOT(1,1)
      R(1,2)=0.0D0*FATOR+DIF_EPOCA*RDOT(1,2)
      R(1,3)=0.0D0*FATOR+DIF_EPOCA*RDOT(1,3)
      R(2,1)=0.0D0*FATOR+DIF_EPOCA*RDOT(2,1)
      R(2,2)=0.0D0*FATOR+DIF_EPOCA*RDOT(2,2)
      R(2,3)=0.0D0*FATOR+DIF_EPOCA*RDOT(2,3) 
      R(3,1)=0.0D0*FATOR+DIF_EPOCA*RDOT(3,1)
      R(3,2)=0.0D0*FATOR+DIF_EPOCA*RDOT(3,2)
      R(3,3)=0.0D0*FATOR+DIF_EPOCA*RDOT(3,3)  
c     CALCULO DAS COORDENADAS      
      CALL ADDAB(X_14,T,A1,3,1,3,1)
      CALL MATSCA(D,X_14,A2,3,1,3,1)  
      CALL AB(R,X_14,A3,3,3,1,3,3,1)
      CALL ADDAB(A1,A2,A4,3,1,3,1)
      CALL ADDAB(A4,A3,X_08,3,1,3,1)
c     CALCULO DAS VELOCIDADES  
      CALL ADDAB(XDOT_14,TDOT,A1DOT,3,1,3,1) 
      CALL MATSCA(DDOT,X_14,A2DOT,3,1,3,1) 
      CALL AB(RDOT,X_14,A3DOT,3,3,1,3,3,1)
      CALL ADDAB(A1DOT,A2DOT,A4DOT,3,1,3,1)
      CALL ADDAB(A4DOT,A3DOT,XDOT_08,3,1,3,1)
      
      
      END                             

      SUBROUTINE I08_I05(X_08,XDOT_08,EPOCA,X_05,XDOT_05)                          
      IMPLICIT NONE
      REAL*8 X_08(3,1),XDOT_08(3,1),X_05(3,1),XDOT_05(3,1),EPOCA
      REAL*8 T(3,1),D,R(3,3),TDOT(3,1),DDOT,RDOT(3,3),DIF_EPOCA
      REAL*8 A1(3,1),A2(3,1),A3(3,1),A4(3,1),FATOR,PI
      REAL*8 A1DOT(3,1),A2DOT(3,1),A3DOT(3,1),A4DOT(3,1) 
      PI= 4.D0*DATAN(1.D0)
      FATOR=(1.0D-3/3600)*(PI/180)
      
C     RATES - PARAMETERS
      TDOT(1,1)=0.3D-3
      TDOT(2,1)=0.0D-3
      TDOT(3,1)=0.0D-3
      DDOT=0.0D-9
      RDOT(1,1)=0.0D0*FATOR
      RDOT(1,2)=0.0D0*FATOR
      RDOT(1,3)=0.0D0*FATOR
      RDOT(2,1)=0.0D0*FATOR
      RDOT(2,2)=0.0D0*FATOR
      RDOT(2,3)=0.0D0*FATOR
      RDOT(3,1)=0.0D0*FATOR
      RDOT(3,2)=0.0D0*FATOR
      RDOT(3,3)=0.0D0*FATOR
c     DIFERENCAS DE EPOCAS
      DIF_EPOCA=EPOCA-2005.0D0
c     CALCULO DOS PARAMETROS ATUALIZADOS      
      T(1,1)=-0.5D-3+DIF_EPOCA*TDOT(1,1)
      T(2,1)=-0.9D-3+DIF_EPOCA*TDOT(2,1)
      T(3,1)=-4.7D-3+DIF_EPOCA*TDOT(3,1)
      D=0.94D-9+DIF_EPOCA*DDOT   
      R(1,1)=0.0D0*FATOR+DIF_EPOCA*RDOT(1,1)
      R(1,2)=0.0D0*FATOR+DIF_EPOCA*RDOT(1,2)
      R(1,3)=0.0D0*FATOR+DIF_EPOCA*RDOT(1,3)
      R(2,1)=0.0D0*FATOR+DIF_EPOCA*RDOT(2,1)
      R(2,2)=0.0D0*FATOR+DIF_EPOCA*RDOT(2,2)
      R(2,3)=0.0D0*FATOR+DIF_EPOCA*RDOT(2,3) 
      R(3,1)=0.0D0*FATOR+DIF_EPOCA*RDOT(3,1)
      R(3,2)=0.0D0*FATOR+DIF_EPOCA*RDOT(3,2)
      R(3,3)=0.0D0*FATOR+DIF_EPOCA*RDOT(3,3)  
c     CALCULO DAS COORDENADAS      
      CALL ADDAB(X_08,T,A1,3,1,3,1)
      CALL MATSCA(D,X_08,A2,3,1,3,1)  
      CALL AB(R,X_08,A3,3,3,1,3,3,1)
      CALL ADDAB(A1,A2,A4,3,1,3,1)
      CALL ADDAB(A4,A3,X_05,3,1,3,1)
c     CALCULO DAS VELOCIDADES  
      CALL ADDAB(XDOT_08,TDOT,A1DOT,3,1,3,1) 
      CALL MATSCA(DDOT,X_08,A2DOT,3,1,3,1) 
      CALL AB(RDOT,X_08,A3DOT,3,3,1,3,3,1)
      CALL ADDAB(A1DOT,A2DOT,A4DOT,3,1,3,1)
      CALL ADDAB(A4DOT,A3DOT,XDOT_05,3,1,3,1)

      
      END

      SUBROUTINE I05_I00(X_05,XDOT_05,EPOCA,X_00,XDOT_00)                          
      IMPLICIT NONE
      REAL*8 X_05(3,1),XDOT_05(3,1),X_00(3,1),XDOT_00(3,1),EPOCA
      REAL*8 T(3,1),D,R(3,3),TDOT(3,1),DDOT,RDOT(3,3),DIF_EPOCA
      REAL*8 A1(3,1),A2(3,1),A3(3,1),A4(3,1),FATOR,PI
      REAL*8 A1DOT(3,1),A2DOT(3,1),A3DOT(3,1),A4DOT(3,1) 
      PI= 4.D0*DATAN(1.D0)
      FATOR=(1.0D-3/3600)*(PI/180)
      
C     RATES - PARAMETERS
      TDOT(1,1)=-0.2D-3
      TDOT(2,1)=0.1D-3
      TDOT(3,1)=-1.8D-3
      DDOT=0.08D-9
      RDOT(1,1)=0.0D0*FATOR
      RDOT(1,2)=0.0D0*FATOR
      RDOT(1,3)=0.0D0*FATOR
      RDOT(2,1)=0.0D0*FATOR
      RDOT(2,2)=0.0D0*FATOR
      RDOT(2,3)=0.0D0*FATOR
      RDOT(3,1)=0.0D0*FATOR
      RDOT(3,2)=0.0D0*FATOR
      RDOT(3,3)=0.0D0*FATOR
c     DIFERENCAS DE EPOCAS
      DIF_EPOCA=EPOCA-2000.0D0
c     CALCULO DOS PARAMETROS ATUALIZADOS      
      T(1,1)=0.1D-3+DIF_EPOCA*TDOT(1,1)
      T(2,1)=-0.8D-3+DIF_EPOCA*TDOT(2,1)
      T(3,1)=-5.8D-3+DIF_EPOCA*TDOT(3,1)
      D=0.40D-9+DIF_EPOCA*DDOT   
      R(1,1)=0.0D0*FATOR+DIF_EPOCA*RDOT(1,1)
      R(1,2)=0.0D0*FATOR+DIF_EPOCA*RDOT(1,2)
      R(1,3)=0.0D0*FATOR+DIF_EPOCA*RDOT(1,3)
      R(2,1)=0.0D0*FATOR+DIF_EPOCA*RDOT(2,1)
      R(2,2)=0.0D0*FATOR+DIF_EPOCA*RDOT(2,2)
      R(2,3)=0.0D0*FATOR+DIF_EPOCA*RDOT(2,3) 
      R(3,1)=0.0D0*FATOR+DIF_EPOCA*RDOT(3,1)
      R(3,2)=0.0D0*FATOR+DIF_EPOCA*RDOT(3,2)
      R(3,3)=0.0D0*FATOR+DIF_EPOCA*RDOT(3,3)  
c     CALCULO DAS COORDENADAS      
      CALL ADDAB(X_05,T,A1,3,1,3,1)
      CALL MATSCA(D,X_05,A2,3,1,3,1)  
      CALL AB(R,X_05,A3,3,3,1,3,3,1)
      CALL ADDAB(A1,A2,A4,3,1,3,1)
      CALL ADDAB(A4,A3,X_00,3,1,3,1)
c     CALCULO DAS VELOCIDADES  
      CALL ADDAB(XDOT_05,TDOT,A1DOT,3,1,3,1) 
      CALL MATSCA(DDOT,X_05,A2DOT,3,1,3,1) 
      CALL AB(RDOT,X_05,A3DOT,3,3,1,3,3,1)
      CALL ADDAB(A1DOT,A2DOT,A4DOT,3,1,3,1)
      CALL ADDAB(A4DOT,A3DOT,XDOT_00,3,1,3,1)

      END
      
      
      SUBROUTINE I00_E00(X_00,XDOT_00,EPOCA,X_E00,XDOT_E00)                        
      IMPLICIT NONE
      REAL*8 X_00(3,1),XDOT_00(3,1),X_E00(3,1),XDOT_E00(3,1),EPOCA
      REAL*8 T(3,1),D,R(3,3),TDOT(3,1),DDOT,RDOT(3,3),DIF_EPOCA
      REAL*8 A1(3,1),A2(3,1),A3(3,1),A4(3,1),FATOR,PI,R1,R2,R3
      REAL*8 A1DOT(3,1),A2DOT(3,1),A3DOT(3,1),A4DOT(3,1) 
      PI= 4.D0*DATAN(1.D0)
      FATOR=(1.0D-3/3600)*(PI/180)
      
C     RATES - PARAMETERS
      TDOT(1,1)=0.0D-3
      TDOT(2,1)=0.0D-3
      TDOT(3,1)=0.0D-3
      DDOT=0.00D-9
      R1=0.081D0
      R2=0.490D0
      R3=-0.792D0
      RDOT(1,1)=0.0D0*FATOR
      RDOT(1,2)=-R3*FATOR
      RDOT(1,3)=R2*FATOR
      RDOT(2,1)=R3*FATOR
      RDOT(2,2)=0.0D0*FATOR
      RDOT(2,3)=-R1*FATOR
      RDOT(3,1)=-R2*FATOR
      RDOT(3,2)=R1*FATOR
      RDOT(3,3)=0.0D0*FATOR
c     DIFERENCAS DE EPOCAS
      DIF_EPOCA=EPOCA-1989.0D0
c     CALCULO DOS PARAMETROS ATUALIZADOS      
      T(1,1)=54.D-3+DIF_EPOCA*TDOT(1,1)
      T(2,1)=51.0D-3+DIF_EPOCA*TDOT(2,1)
      T(3,1)=-48.0D-3+DIF_EPOCA*TDOT(3,1)
      D=0.00D-9+DIF_EPOCA*DDOT   
      R(1,1)=0.0D0*FATOR+DIF_EPOCA*RDOT(1,1)
      R(1,2)=0.0D0*FATOR+DIF_EPOCA*RDOT(1,2)
      R(1,3)=0.0D0*FATOR+DIF_EPOCA*RDOT(1,3)
      R(2,1)=0.0D0*FATOR+DIF_EPOCA*RDOT(2,1)
      R(2,2)=0.0D0*FATOR+DIF_EPOCA*RDOT(2,2)
      R(2,3)=0.0D0*FATOR+DIF_EPOCA*RDOT(2,3) 
      R(3,1)=0.0D0*FATOR+DIF_EPOCA*RDOT(3,1)
      R(3,2)=0.0D0*FATOR+DIF_EPOCA*RDOT(3,2)
      R(3,3)=0.0D0*FATOR+DIF_EPOCA*RDOT(3,3)  
c     CALCULO DAS COORDENADAS      
      CALL ADDAB(X_00,T,A1,3,1,3,1)
      CALL MATSCA(D,X_00,A2,3,1,3,1)  
      CALL AB(R,X_00,A3,3,3,1,3,3,1)
      CALL ADDAB(A1,A2,A4,3,1,3,1)
      CALL ADDAB(A4,A3,X_E00,3,1,3,1)
c     CALCULO DAS VELOCIDADES  
      CALL ADDAB(XDOT_00,TDOT,A1DOT,3,1,3,1) 
      CALL MATSCA(DDOT,X_00,A2DOT,3,1,3,1) 
      CALL AB(RDOT,X_00,A3DOT,3,3,1,3,3,1)
      CALL ADDAB(A1DOT,A2DOT,A4DOT,3,1,3,1)
      CALL ADDAB(A4DOT,A3DOT,XDOT_E00,3,1,3,1)

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
      

