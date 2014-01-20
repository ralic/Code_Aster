subroutine ornorm(noma, listma, nbmail, reorie, norien)
    implicit none
#include "jeveux.h"
#include "asterfort/indiis.h"
#include "asterfort/infniv.h"
#include "asterfort/iorim1.h"
#include "asterfort/iorim2.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmavo.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    integer :: listma(*), nbmail, norien
    logical :: reorie
    character(len=8) :: noma
!.======================================================================
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
!
!   ORNORM  --  LE BUT EST QUE TOUTES LES MAILLES DE LA LISTE SOIENT
!               ORIENTEES COMME LA PREMIERE MAILLE DE LA LISTE.
!
!   ARGUMENT        E/S  TYPE         ROLE
!    NOMA           IN    K8      NOM DU MAILLAGE
!    LISTMA         IN    I       LISTE DES MAILLES A REORIENTER
!    NBMAIL         IN    I       NB DE MAILLES DE LA LISTE
!    NORIEN        VAR            NOMBRE DE MAILLES REORIENTEES
!.========================= DEBUT DES DECLARATIONS ====================
! -----  VARIABLES LOCALES
    integer :: idtyma, nutyma,     iliste
    integer :: ima, numail, numa, norieg, lliste
    integer :: im1, im2, ico, ibid(1)
    integer :: p1, p2, ifm, niv,  p3, p4
    integer :: jdesm1, jdesm2
    integer :: nbmavo, indi, im3, nconex, zero
    logical :: dime1, dime2
    character(len=1) :: lect
    character(len=2) :: kdim
    character(len=8) :: typel, nomail
    character(len=24) :: mailma, nomavo
    character(len=24) :: valk(2)
    integer, pointer :: ori1(:) => null()
    integer, pointer :: ori2(:) => null()
    integer, pointer :: ori3(:) => null()
    integer, pointer :: ori4(:) => null()
    character(len=8), pointer :: ori5(:) => null()
!
#define pasori(ima) ori1(ima).eq.0
!
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
    call jemarq()
    if (nbmail .eq. 0) goto 9999
!
    call infniv(ifm, niv)
!
    zero = 0
    mailma = noma//'.NOMMAI'
    lect = 'L'
    if (reorie) lect = 'E'
!
! --- VECTEUR DU TYPE DES MAILLES DU MAILLAGE :
!     ---------------------------------------
    call jeveuo(noma//'.TYPMAIL', 'L', idtyma)
!
! --- APPEL A LA CONNECTIVITE :
!     -----------------------
    call jeveuo(jexatr(noma//'.CONNEX', 'LONCUM'), 'L', p2)
    call jeveuo(noma//'.CONNEX', lect, p1)
!
!     ALLOCATIONS :
!     -----------
    AS_ALLOCATE(vi=ori1, size=nbmail)
    AS_ALLOCATE(vi=ori2, size=nbmail)
    AS_ALLOCATE(vi=ori3, size=nbmail)
    AS_ALLOCATE(vi=ori4, size=nbmail)
    AS_ALLOCATE(vk8=ori5, size=nbmail)
!
! --- VERIFICATION DU TYPE DES MAILLES
! --- (ON DOIT AVOIR DES MAILLES DE PEAU) :
!     -----------------------------------
    dime1 = .false.
    dime2 = .false.
    do 10 ima = 1, nbmail
        ori1(ima) = 0
        numa = listma(ima)
        ori3(ima) = zi(p2+numa)-zi(p2-1+numa)
        ori4(ima) = zi(p2+numa-1)
        jdesm1 = zi(p2+numa-1)
!
! ---   TYPE DE LA MAILLE COURANTE :
!       --------------------------
        nutyma = zi(idtyma+numa-1)
        call jenuno(jexnum('&CATA.TM.NOMTM', nutyma), typel)
        ori5(ima) = typel
!
        if (typel(1:4) .eq. 'QUAD') then
            dime2 = .true.
        else if (typel(1:4).eq.'TRIA') then
            dime2 = .true.
        else if (typel(1:3).eq.'SEG') then
            dime1 = .true.
        else
            call jenuno(jexnum(mailma, numa), nomail)
            valk(1) = nomail
            valk(2) = typel
            call utmess('F', 'MODELISA5_94', nk=2, valk=valk)
        endif
        if (dime1 .and. dime2) then
            call utmess('F', 'MODELISA5_98')
        endif
10  end do
!
! --- RECUPERATION DES MAILLES VOISINES DU GROUP_MA :
!     ---------------------------------------------
    kdim ='  '
    if (dime1) kdim ='1D'
    if (dime2) kdim ='2D'
    nomavo = '&&ORNORM.MAILLE_VOISINE '
    call utmavo(noma, kdim, listma, nbmail, 'V',&
                nomavo, zero, ibid)
    call jeveuo(jexatr(nomavo, 'LONCUM'), 'L', p4)
    call jeveuo(nomavo, 'L', p3)
!
    norieg = 0
!
! --- LA BOUCLE 100 DEFINIT LES CONNEXES
!
    nconex = 0
    do 100 ima = 1, nbmail
        numail = listma(ima)
! ----- SI LA MAILLE N'EST PAS ORIENTEE ON L'ORIENTE
        if (pasori(ima)) then
            if (niv .eq. 2) then
                call jenuno(jexnum(mailma, numail), nomail)
                write (ifm,*) 'LA MAILLE ',nomail,&
     &                    ' SERT A ORIENTER UN NOUVEAU GROUPE CONNEXE'
            endif
            nconex = nconex + 1
            if (nconex .gt. 1) then
                call utmess('F', 'MODELISA5_99')
            endif
            ori1(ima) = 1
            lliste = 0
            iliste = 0
            ori2(lliste+1) = ima
!
! ------- ON ORIENTE TOUTES LES MAILLES DU CONNEXE
!
200          continue
!
            im1 = ori2(iliste+1)
            jdesm1 = ori4(im1)
! ------- ON ESSAYE D'ORIENTER LES MAILLES VOISINES
            nbmavo = zi(p4+im1)-zi(p4-1+im1)
            do 210 im3 = 1, nbmavo
                indi = zi(p3+zi(p4+im1-1)-1+im3-1)
                im2 = indiis ( listma, indi, 1, nbmail )
                if (im2 .eq. 0) goto 210
                numail = listma(im2)
                if (pasori(im2)) then
                    jdesm2 = ori4(im2)
!             VERIFICATION DE LA CONNEXITE ET REORIENTATION EVENTUELLE
                    if (dime1) ico = iorim1 ( zi(p1+jdesm1-1), zi(p1+jdesm2-1), reorie)
                    if (dime2) ico = iorim2 (&
                                     zi(p1+jdesm1-1), ori3(im1), zi(p1+jdesm2-1),&
                                     ori3(im2), reorie&
                                     )
!             SI MAILLES CONNEXES
                    if (ico .ne. 0) then
                        ori1(im2) = 1
                        lliste = lliste + 1
                        ori2(lliste+1) = im2
                        if (reorie .and. niv .eq. 2) then
                            call jenuno(jexnum(mailma, numail), nomail)
                            if (ico .lt. 0) then
                                write (ifm,*) 'LA MAILLE ',nomail,' A ETE REORIENTEE'
                            else
                                write (ifm,*) 'LA MAILLE ',nomail,' EST ORIENTEE'
                            endif
                        endif
                    endif
!
!             SI ORIENTATIONS CONTRAIRES
                    if (ico .lt. 0) norieg = norieg + 1
!
                endif
210          continue
            iliste = iliste + 1
            if (iliste .le. lliste) goto 200
        endif
100  end do
!
    norien = norien + norieg
!
    AS_DEALLOCATE(vi=ori1)
    AS_DEALLOCATE(vi=ori2)
    AS_DEALLOCATE(vi=ori3)
    AS_DEALLOCATE(vi=ori4)
    AS_DEALLOCATE(vk8=ori5)
    call jedetr(nomavo)
!
9999  continue
    call jedema()
end subroutine
