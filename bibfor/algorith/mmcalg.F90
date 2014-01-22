subroutine mmcalg(ndim  ,nnm   ,    &
          ddffm ,geomam, &
          tau1  ,tau2  ,jeu   ,norm  , &
          gene11,gene21,gene22,kappa ,h        , &
          vech1 ,vech2 ,a     ,ha    ,hah   , &
          mprt11,mprt21,mprt22,mprt1n,mprt2n,iresog)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit     none
#include "asterfort/assert.h"   
#include "asterfort/matini.h"
#include "asterfort/vecini.h"
#include "asterfort/matinv.h"
    
    integer :: ndim,  nnm

    integer :: iresog
    
    

    real(kind=8) :: geomam(9, 3)



    real(kind=8) :: jeu
    

    real(kind=8) :: ddffm(3,9)
    
    real(kind=8) :: norm(3), tau1(3), tau2(3)
    
    real(kind=8) :: mprt1n(3, 3), mprt2n(3, 3)
    real(kind=8) :: mprt11(3, 3), mprt21(3, 3), mprt22(3, 3)
       
    real(kind=8) :: gene11(3, 3), gene21(3, 3),gene22(3,3)
    
    real(kind=8) :: kappa(2,2),a(2,2),h(2,2),ha(2,2),hah(2,2)
    
    real(kind=8) :: vech1(3),vech2(3) 
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
!
! CALCUL DES MATRICES DE PROJECTION POUR NEWTON GENERALISE
!
! ----------------------------------------------------------------------
! IN  NDIM   : DIMENSION DE LA MAILLE DE CONTACT
! IN  NNE    : NOMBRE DE NOEUDS DE LA MAILLE ESCLAVE
! IN  NNM    : NOMBRE DE NOEUDS DE LA MAILLE MAITRE
! IN  IRESOG : ALGO. DE RESOLUTION POUR LA GEOMETRIE
!              0 - POINT FIXE
!              1 - NEWTON
! IN FFE    : FONCTIONS DE FORMES DEPL. ESCL.
! IN FFM    : FONCTIONS DE FORMES DEPL. MAIT.
! IN DFFM   : DERIVEES PREMIERE DES FONCTIONS DE FORME MAITRES 
! IN DDFFM  : DERIVEES SECONDES DES FONCTIONS DE FORME MAITRES
! IN GEOMAE : GEOMETRIE ACTUALISEE SUR NOEUDS ESCLAVES
! IN GEOMAM : GEOMETRIE ACTUALISEE SUR NOEUDS MAITRES
! IN tau1   : PREMIER VECTEUR TANGENT
! IN tau2   : SECOND VECTEUR TANGENT
! IN NORM   : NORMALE INTERIEURE
! IN GEOME  : COORDONNEES ACTUALISEES DU POINT DE CONTACT
! IN GEOMM  : COORDONNEES ACTUALISEES DU PROJETE DU POINT DE CONTACT

! -----------------------------------------------------------------------
!     MATRICES ISSUES DE LA GEOMETRIE DIFFERENTIELLE POUR NEWTON GENE
! -----------------------------------------------------------------------
! OUT MPRT11 : MATRICE DE PROJECTION TANGENTE DANS LA DIRECTION 1
! OUT MPRT11 = tau1*TRANSPOSE(tau1)(matrice 3*3)
!
! OUT MPRT12 : MATRICE DE PROJECTION TANGENTE DANS LA DIRECTION 1/2
! OUT MPRT12 = tau1*TRANSPOSE(tau2)(matrice 3*3)
!
! OUT MPRT12 : MATRICE DE PROJECTION TANGENTE DANS LA DIRECTION 2
! OUT MPRT12 = tau2*TRANSPOSE(tau2)(matrice 3*3)
!
! OUT GENE11 : MATRICE EULERIENNE DUE A LA REGULARITE DE LA SURFACE MAITRE
! OUT          DDGEOMM(1,1)/NORMALE  (matrice 3*3)
!
! OUT GENE21 : MATRICE EULERIENNE DUE A LA REGULARITE DE LA SURFACE MAITRE
! OUT          DDGEOMM(2,1)/NORMALE  (matrice 3*3) 
!
! OUT GENE22 : MATRICE EULERIENNE DUE A LA REGULARITE DE LA SURFACE MAITRE
! OUT          DDGEOMM(2,2)/NORMALE  (matrice 3*3)
!
! OUT KAPPA  : MATRICE DE SCALAIRES LIEES A LA CINEMATIQUE DU GLISSEMENT
! OUT KAPPA(i,j) = INVERSE[tau_i.tau_j-JEU*(ddFFM*geomm)](matrice 2*2)
!
! OUT H : MATRICE DE SCALAIRES EULERIENNE DUE A LA REGULARITE DE LA SURFACE MAITRE
! OUT H(i,j) = JEU*{[DDGEOMM(i,j)].n} (matrice 2*2)
!
! OUT A : MATRICE DE SCALAIRES DUE AU TENSEUR DE WEINTGARTEN
! OUT A(i,j) = [tau_i.tau_j] (matrice 2*2)
! 
! OUT HA  = H/A   (matrice 2*2)
! OUT HAH = HA/H  (matrice 2*2)
! 
! OUT VECH_i = KAPPA(i,m)*tau_m
!
! OUT IRESOG EST REMIS AUTOMATIQUEMENT A ZERO SI DET(KAPPA) = 0 
!              0 - POINT FIXE
!              1 - NEWTON
! ----------------------------------------------------------------------

!
! ----------------------------------------------------------------------
!
    integer :: i, j, inom,idim
    real(kind=8) :: ddgeo1(3),ddgeo2(3),ddgeo3(3),detkap
!
! ----------------------------------------------------------------------
!
    call matini(3, 3, 0.d0, mprt1n)
    call matini(3, 3, 0.d0, mprt2n)
    
    call matini(3, 3, 0.d0, mprt11)
    call matini(3, 3, 0.d0, mprt21)
    call matini(3, 3, 0.d0, mprt22)
    
    call matini(2, 2, 0.d0, h)
    call matini(2, 2, 0.d0, ha)
    call matini(2, 2, 0.d0, hah)
        
    call matini(3, 3, 0.d0, gene11)
    call matini(3, 3, 0.d0, gene21)
    call matini(3, 3, 0.d0, gene22)
    
    call matini(2, 2 ,0.d0, kappa ) 
    
    call vecini(3, 0.d0, vech1)
    call vecini(3, 0.d0, vech2)
    
    call vecini(3, 0.d0, ddgeo1)
    call vecini(3, 0.d0, ddgeo2)
    call vecini(3, 0.d0, ddgeo3)  

!
!---CALCUL DE DDGEOMM
!


      do  idim = 1, ndim
          do 222 inom = 1, nnm

            ddgeo1(idim) = ddgeo1(idim) + ddffm(1,inom)*geomam(inom,idim)
            ddgeo2(idim) = ddgeo2(idim) + ddffm(2,inom)*geomam(inom,idim)
            ddgeo3(idim) = ddgeo3(idim) + ddffm(3,inom)*geomam(inom,idim)

222        continue
    end do    


!
! --- MATRICE DE PROJECTION TANGENTE1/NORMALE
!

    do  i = 1, ndim
        do 116 j = 1, ndim
            mprt1n(i,j) = 1.d0*tau1(i)*norm(j)
116      continue
  end do


!
! --- MATRICE DE PROJECTION TANGENTE2/NORMALE
!
    do  i = 1, ndim
        do 117 j = 1, ndim
            mprt2n(i,j) = 1.d0*tau2(i)*norm(j)
117      continue
  end do



!
! --- MATRICE DE PROJECTION TANGENTE1/TANGENTE1
!
    do  i = 1, ndim
        do 217 j = 1, ndim
            mprt11(i,j) = 1.d0*tau1(i)*tau1(j)
217      continue
  end do


!
! --- MATRICE DE PROJECTION TANGENTE2/TANGENTE1
!
    do i = 1, ndim
        do 317 j = 1, ndim
            mprt21(i,j) = 1.d0*tau2(i)*tau1(j)
317      continue
  end do



!
! --- MATRICE DE PROJECTION TANGENTE2/TANGENTE2
!
    do  i = 1, ndim
        do 417 j = 1, ndim
            mprt22(i,j) = 1.d0*tau2(i)*tau2(j)
417      continue
  end do


    do  i = 1, ndim
        do 717 j = 1, ndim
            mprt1n(i,j) = 1.d0*tau1(i)*norm(j)
717      continue
  end do

    do  i = 1, ndim
        do 817 j = 1, ndim
            mprt2n(i,j) = 1.d0*tau2(i)*norm(j)
817      continue
  end do

!
! --- MATRICE GENE11,GENE21,GENE22
!
    do  i = 1, ndim
        do 25 j = 1, ndim
            do 27 inom = 1, nnm
                gene11(i,j) = gene11(i,j)+ ddffm(1,inom)*geomam(inom, i)*norm(j)
                gene22(i,j) = gene11(i,j)+ ddffm(2,inom)*geomam(inom, i)*norm(j)
                gene21(i,j) = gene21(i,j)+ ddffm(3,inom)*geomam(inom, i)*norm(j)
27          continue
25      continue
  end do



!
! --- MATRICES KAPPA
!

    if (ndim .eq. 2) then
    

      kappa(1,1) = ddgeo1(1)*norm(1)+ddgeo1(2)*norm(2)+ddgeo1(3)*norm(3)      
      kappa(1,1) = kappa(1,1)+tau1(1)*tau1(1)+tau1(2)*tau1(2)+tau1(3)*tau1(3)  

        if (kappa(1,1) .ge. 1.0d-16) then
          kappa(1,1) = 1.0d0/kappa(1,1)
    else
      iresog=0
          !ON NE CALCULE PAS LES MATRICES DE NEWTON GENE POUR CET ELEMENT 
    endif                
      
    else if (ndim.eq.3) then


      kappa(1,1) = ddgeo1(1)*norm(1)+ddgeo1(2)*norm(2)+ddgeo1(3)*norm(3) 
      kappa(1,1) = kappa(1,1)+tau1(1)*tau1(1)+tau1(2)*tau1(2)+tau1(3)*tau1(3)      


      kappa(1,2) = ddgeo3(1)*norm(1)+ddgeo3(2)*norm(2)+ddgeo3(3)*norm(3)     
      kappa(1,2) = kappa(1,2)+tau1(1)*tau2(1)+tau1(2)*tau2(2)+tau1(3)*tau2(3)  
      

      kappa(2,1) = kappa(1,2)


      kappa(1,1) = ddgeo2(1)*norm(1)+ddgeo2(2)*norm(2)+ddgeo2(3)*norm(3)
      kappa(2,2) = kappa(2,2)+tau2(1)*tau2(1)+tau2(2)*tau2(2)+tau2(3)*tau2(3)



      call matinv('C',2,kappa,  kappa,detkap)
      
      if (detkap .le.  1.0d-16) then
          iresog = 0
         !ON NE CALCULE PAS LES MATRICES DE NEWTON GENE POUR CET ELEMENT  
     
     
      endif

    else
        ASSERT(.false.)         
    endif

!
! --- MATRICES A
!

     a(1,1) = tau1(1)*tau1(1)+tau1(2)*tau1(2)+tau1(3)*tau1(3)
     a(1,2) = tau1(1)*tau2(1)+tau1(2)*tau2(2)+tau1(3)*tau2(3)
     a(2,1) = a(1,2)
     a(2,2) = tau2(1)*tau2(1)+tau2(2)*tau2(2)+tau2(3)*tau2(3)
     
!
! --- MATRICES H
!

     h(1,1) = jeu*(ddgeo1(1)*norm(1)+ddgeo1(2)*norm(2)+ddgeo1(3)*norm(3))
     h(1,2) = jeu*(ddgeo3(1)*norm(1)+ddgeo3(2)*norm(2)+ddgeo3(3)*norm(3))
     h(2,1) =      h(1,2) 
     h(2,2) = jeu*(ddgeo2(1)*norm(1)+ddgeo2(2)*norm(2)+ddgeo2(3)*norm(3))

!
! --- MATRICES HA
!
     ha(1,1) = h(1,1)*a(1,1)+h(1,2)*a(2,1)
     ha(1,2) = h(1,1)*a(1,2)+h(1,2)*a(2,2)
     ha(2,1) = h(2,1)*a(1,1)+h(2,2)*a(2,1)    
     ha(2,2) = h(2,1)*a(1,2)+h(2,2)*a(2,2)

!
! --- MATRICES HAH
!

     hah(1,1) = ha(1,1)*h(1,1)+ha(1,2)*h(2,1)
     hah(1,2) = ha(1,1)*h(1,2)+ha(1,2)*h(2,2)
     hah(2,1) = ha(2,1)*h(1,1)+ha(2,2)*h(2,1)    
     hah(2,2) = ha(2,1)*h(1,2)+ha(2,2)*h(2,2)


!
! --- VECTEUR VECH1
!
     
     vech1(1) = kappa(1,1)*tau1(1)+kappa(1,2)*tau2(1)
     vech1(2) = kappa(1,1)*tau1(2)+kappa(1,2)*tau2(2)
     vech1(3) = kappa(1,1)*tau1(3)+kappa(1,2)*tau2(3)

!
! --- VECTEUR VECH2
!

     vech2(1) = kappa(2,1)*tau1(1)+kappa(1,2)*tau2(1)
     vech2(2) = kappa(2,1)*tau1(2)+kappa(1,2)*tau2(2)
     vech2(3) = kappa(2,1)*tau1(3)+kappa(1,2)*tau2(3)



end subroutine
