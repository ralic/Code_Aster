subroutine orvlma(noma, listma, nbmail, norien, vect,&
                  noeud)
    implicit none
#include "jeveux.h"
#include "asterfort/indiis.h"
#include "asterfort/infniv.h"
#include "asterfort/iorim1.h"
#include "asterfort/iorim2.h"
#include "asterfort/ioriv1.h"
#include "asterfort/ioriv2.h"
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
!
    integer :: listma(*), nbmail, noeud, norien
    character(len=8) :: noma
    real(kind=8) :: vect(*)
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
!   ORVLMA  --  LE BUT EST QUE TOUTES LES MAILLES DE LA LISTE SOIENT
!               ORIENTEES SUIVANT LE VECTEUR DIRECTEUR.
!
!   ARGUMENT        E/S  TYPE         ROLE
!    NOMA           IN    K8      NOM DU MAILLAGE
!    LISTMA         IN    I       LISTE DES MAILLES A REORIENTER
!    NBMAIL         IN    I       NB DE MAILLES DE LA LISTE
!    NORIEN        VAR            NOMBRE DE MAILLES REORIENTEES
!    VECT           IN    R       VECTEUR DIRECTEUR
!    NOEUD          IN    I       NOEUD D'ORIENTATION
!.========================= DEBUT DES DECLARATIONS ====================
! -----  VARIABLES LOCALES
    integer ::  nutyma, lori, jori, nori, kori, iliste
    integer :: ima, numail, numa, norieg, lliste, zero, ibid(1)
    integer :: im1, im2, ico
    integer :: p1, p2, ifm, niv, p3, p4
    integer :: nbnmai, jdesm1, jdesm2
    integer :: nbmavo, indi, im3, jcoor
    integer :: nbmaor, ii, kdeb
    logical :: dime1, dime2, reorie
    character(len=2) :: kdim
    character(len=8) :: typel, nomail
    character(len=24) :: mailma, nomavo
    character(len=24) :: valk(2)
    integer, pointer :: typmail(:) => null()
!
#define pasori(ima) zi(lori-1+ima).eq.0
!
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
    call jemarq()
!
    call infniv(ifm, niv)
    mailma = noma//'.NOMMAI'
    reorie = .true.
    zero = 0
!
! --- VECTEUR DU TYPE DES MAILLES DU MAILLAGE :
!     ---------------------------------------
    call jeveuo(noma//'.TYPMAIL', 'L', vi=typmail)
!
! --- COORDONNEES DES NOEUDS DU MAILLAGE :
!     ----------------------------------
    call jeveuo(noma//'.COORDO    .VALE', 'L', jcoor)
!
! --- APPEL A LA CONNECTIVITE :
!     -----------------------
    call jeveuo(jexatr(noma//'.CONNEX', 'LONCUM'), 'L', p2)
    call jeveuo(noma//'.CONNEX', 'E', p1)
!
!     ALLOCATIONS :
!     -----------
    call wkvect('&&ORVLMA.ORI1', 'V V I', nbmail, lori)
    call wkvect('&&ORVLMA.ORI2', 'V V I', nbmail, jori)
    call wkvect('&&ORVLMA.ORI3', 'V V I', nbmail, nori)
    call wkvect('&&ORVLMA.ORI4', 'V V I', nbmail, kori)
    call wkvect('&&ORVLMA.ORI5', 'V V I', nbmail, kdeb)
!
! --- VERIFICATION DU TYPE DES MAILLES
! --- (ON DOIT AVOIR DES MAILLES DE PEAU) :
!     -----------------------------------
    dime1 = .false.
    dime2 = .false.
    do 10 ima = 1, nbmail
        zi(lori-1+ima) = 0
        numa = listma(ima)
        zi(nori-1+ima) = zi(p2+numa)-zi(p2-1+numa)
        zi(kori-1+ima) = zi(p2+numa-1)
        jdesm1 = zi(p2+numa-1)
!
! ---   TYPE DE LA MAILLE COURANTE :
!       --------------------------
        nutyma = typmail(numa)
        call jenuno(jexnum('&CATA.TM.NOMTM', nutyma), typel)
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
    nomavo = '&&ORVLMA.MAILLE_VOISINE '
    call utmavo(noma, kdim, listma, nbmail, 'V',&
                nomavo, zero, ibid)
    call jeveuo(jexatr(nomavo, 'LONCUM'), 'L', p4)
    call jeveuo(nomavo, 'L', p3)
!
! --- PREMIER PASSAGE: METTRE LES MAILLES AYANT LE NOEUD DANS
!     LA BONNE ORIENTATION
!
    norieg = 0
!
    nbmaor = 0
    do 20 ima = 1, nbmail
        numa = listma(ima)
        nbnmai = zi(nori-1+ima)
        jdesm1 = zi(kori-1+ima)
!
! ------ VERIFICATION QUE LE NOEUD EST DANS LA MAILLE
        if (dime1) ico = ioriv1( zi(p1+jdesm1-1), noeud, vect, zr( jcoor) )
        if (dime2) ico = ioriv2( zi(p1+jdesm1-1),nbnmai, noeud,vect, zr(jcoor))
!
! ------ LA MAILLE NE CONTIENT PAS LE NOEUD
        if (ico .eq. 0) then
!
! ------ LA MAILLE A ETE REORIENTEE
        else if (ico .lt. 0) then
            nbmaor = nbmaor + 1
            zi(kdeb+nbmaor-1) = ima
            zi(lori-1+ima) = 1
            if (niv .eq. 2) then
                call jenuno(jexnum(mailma, numa), nomail)
                write(ifm,*) 'LA MAILLE '//nomail//&
     &                       ' A ETE ORIENTEE PAR RAPPORT AU VECTEUR'
            endif
            norieg = norieg + 1
!
! ------ LA MAILLE A LA BONNE ORIENTATION
        else
            nbmaor = nbmaor + 1
            zi(kdeb+nbmaor-1) = ima
            zi(lori-1+ima) = 1
            if (niv .eq. 2) then
                call jenuno(jexnum(mailma, numa), nomail)
                write(ifm,*) 'LA MAILLE '//nomail//&
     &                       ' EST ORIENTEE PAR RAPPORT AU VECTEUR'
            endif
        endif
!
20  end do
    if (nbmaor .eq. 0) then
        call utmess('F', 'MODELISA6_1')
    endif
!
    do 300 ii = 1, nbmaor
        lliste = 0
        iliste = 0
        zi(jori+lliste) = zi(kdeb+ii-1)
!
! --- ON ORIENTE TOUTES LES MAILLES DU CONNEXE
!
200      continue
!
        im1 = zi(jori+iliste)
        jdesm1 = zi(kori-1+im1)
! --- ON ESSAYE D'ORIENTER LES MAILLES VOISINES
        nbmavo = zi(p4+im1)-zi(p4-1+im1)
        do 210 im3 = 1, nbmavo
            indi = zi(p3+zi(p4+im1-1)-1+im3-1)
            im2 = indiis ( listma, indi, 1, nbmail )
            if (im2 .eq. 0) goto 210
            numail = listma(im2)
            if (pasori(im2)) then
                jdesm2 = zi(kori-1+im2)
!           VERIFICATION DE LA CONNEXITE ET REORIENTATION EVENTUELLE
                if (dime1) ico = iorim1 ( zi(p1+jdesm1-1), zi(p1+ jdesm2-1), reorie)
                if (dime2) ico = iorim2 (&
                                 zi(p1+jdesm1-1), zi(nori-1+ im1), zi(p1+jdesm2-1),&
                                 zi(nori-1+im2), reorie&
                                 )
!           SI MAILLES CONNEXES
                if (ico .ne. 0) then
                    zi(lori-1+im2) = 1
                    lliste = lliste + 1
                    zi(jori+lliste) = im2
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
!           SI ORIENTATIONS CONTRAIRES
                if (ico .lt. 0) norieg = norieg + 1
!
            endif
210      end do
        iliste = iliste + 1
        if (iliste .le. lliste) goto 200
300  end do
!
! --- ON VERIFIE QU'ON A BIEN TRAITE TOUTES LES MAILLES
!
    do 100 ima = 1, nbmail
        if (pasori(ima)) then
            call utmess('F', 'MODELISA6_2')
        endif
100  end do
!
    norien = norien + norieg
!
    call jedetr('&&ORVLMA.ORI1')
    call jedetr('&&ORVLMA.ORI2')
    call jedetr('&&ORVLMA.ORI3')
    call jedetr('&&ORVLMA.ORI4')
    call jedetr('&&ORVLMA.ORI5')
    call jedetr(nomavo)
!
    call jedema()
end subroutine
