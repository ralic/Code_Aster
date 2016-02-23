subroutine testvois(mail,jcoor,jtypma,coorma,typma,nuesco,&
                    tole,ndim,poidstt)


  ! ======================================================================
  ! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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

    implicit none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/apcoor.h"
#include "asterfort/prjint.h"
#include "asterfort/assert.h"
#include "asterfort/dctest.h"
#include "asterf_types.h"


    character(len=8), intent(in) :: mail
    integer,intent(in) :: jcoor, jtypma, nuesco, ndim
    real(kind=8),intent(in) :: coorma(27), tole
    character(len=8),intent(in) :: typma
    real(kind=8),intent(out) :: poidstt

  ! ----------------------------------------------------------------------
  !         Test de la validité d'un voisin pour etre une nouvelle  
  !         maille de depart
  ! ----------------------------------------------------------------------
  ! IN          MAIL       MAILLAGE
  ! IN         COORMA      COORDONNÉE DES POINT DE LA MAILLE MAITRE
  ! IN         TYPMA       TYPE DE MAILLE MAITRE
  ! IN         NUES        NUMERO DE LA MAILLE ESCLAVE
  ! IN         TOLE        TOLERANCE
  ! IN         NDIM        DIMENSION ESPACE MAILLAGE MAITRE
  ! OUT        POIDSTT     POIDS DE L INTERSECTION ENTRE LA MAILLE MAITRE ET ESCLAVE
  ! ----------------------------------------------------------------------

  !---- VARIABLES LOCALES -----------------------------------------------
    integer:: ind3, ind4, ind5, idim

  !---- VARIABLES RECUPERATION COORDONNEES ------------------------------
    integer ::  nbnes, ndimes
    real(kind=8) ::  coores(27)
    character(len=8) ::types

  !---- VARIABLES APPROXIMATION GEO ---------------------------------------
    integer :: lisses(8,9), lissma(8,9)
    integer :: nbnses(8), nbnsma(8)
    integer :: nbsses, nbssma 
    real(kind=8) :: crsses(27), crssma(27)
    character(len=8) :: tpsses, tpssma

  !---- VARIABLES PROJECTION --------------------------------------------
    integer ::  nbpint, itvaux(4)
    real(kind=8) :: poids
    real(kind=8) :: corint(32)


    call jemarq()
!
! 
    if(nuesco .eq. 0) then
        go to 100
    end if  

  ! --- Récuparation coordonnée maille esclave courante -----------------------
    call apcoor(mail, jcoor, jtypma, nuesco, coores,&
                nbnes, types, ndimes)

  ! ---- Approximation geometrie esclave linearisee ---------------------------
    call dctest(types, lisses, nbnses, nbsses,tpsses)
    
  ! ---- Approximation geometrie maitre linearisee ---------------------------
    call dctest(typma, lissma, nbnsma, nbssma,tpssma)  


    poidstt=0.d0

! ------------ Boucle sur les sous-mailles --------------------------------
    do ind5=1, nbsses
!                
        do ind4=1, nbnses(ind5)
            do idim=1,ndim
                crsses((ind4-1)*3+idim) = coores((lisses(ind5,ind4)-1)*3+idim) 
            end do 
        end do
!                
        do ind3=1, nbssma
!             
            do ind4=1, nbnsma(ind3)
                do idim=1,ndim
                    crssma((ind4-1)*3+idim) = coorma((lissma(ind3,ind4)-1)*3+idim) 
                end do 
            end do
! ------------ Projection Intersection -----------------------------------------------------------
            poids=0.d0
            call prjint(crsses,nbnses(ind5),tpsses,crssma,nbnsma(ind3),tpssma,&
                                corint,poids,nbpint,itvaux, tole ,ndim)                         
            poidstt=poidstt+poids
        end do      
    end do
    100 continue

    call jedema()

end subroutine testvois
