subroutine axdipo(noma, caelem, modele, iaxe)
    implicit none
#include "jeveux.h"
#include "asterc/indik8.h"
#include "asterc/r8prem.h"
#include "asterfort/dismoi.h"
#include "asterfort/etenca.h"
#include "asterfort/exisdg.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetc.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/locglo.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    integer :: iaxe
    character(len=8) :: noma, caelem, modele
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!-----------------------------------------------------------------------
!     DETERMINATION DE L'AXE DIRECTEUR D'UNE POUTRE DROITE
!     APPELANT : SPECFF
!-----------------------------------------------------------------------
! IN  : NOMA   : NOM DU CONCEPT MAILLAGE
! IN  : CAELEM : NOM DU CONCEPT CARA_ELEM
! IN  : MODELE : NOM DU CONCEPT MODELE
! OUT : IAXE   : ENTIER DEFINISSANT L'AXE DIRECTEUR
!       IAXE = 1 L'AXE DIRECTEUR EST L'AXE DES X DU REPERE GLOBAL
!       IAXE = 2 L'AXE DIRECTEUR EST L'AXE DES Y DU REPERE GLOBAL
!       IAXE = 3 L'AXE DIRECTEUR EST L'AXE DES Z DU REPERE GLOBAL
!
!
!
!-----------------------------------------------------------------------
    integer :: nbtel, nbtel1
    parameter     (nbtel=9,nbtel1=7)
!
    integer :: ialpha, iangl, ias, iasmax, iaxe2, ibeta, icaori
    integer :: icmp, icode, idesc, iexcar, igamma, itelok, ipbl
    integer :: iptma, irana, iranb, irang, iret, itel
    integer :: ivale, ixnw, jdme, nbec, nbmail, nbmtrd
    integer :: ncmpor, nummai, nutyel, pobali, iaux1
    integer :: ntyele(nbtel)
!
    logical(kind=1) :: ntrouv
!
    real(kind=8) :: alpha, alpha2, beta, beta2, cosa, cosa2, cosb
    real(kind=8) :: cosb2, cosg, cosg2, dif1, dif2, dif3, dife1
    real(kind=8) :: dife2, dife3, gamma, gamma2, sina, sina2
    real(kind=8) :: sinb, sinb2, sing, sing2, tol
    real(kind=8) :: vdl(3), vdg(3)
!-----------------------------------------------------------------------
    character(len=8) :: nomcmp(3)
    character(len=16) :: nomele(nbtel)
    character(len=19) :: carte, ligrmo
    character(len=24) :: modmai, modnem, cadesc, captma, cavale
    character(len=32) :: kexnom
!
    data nomcmp/'ALPHA   ','BETA    ','GAMMA   '/
!
    data nomele/'MECA_POU_D_T    ','MECA_POU_D_E    ',&
     &            'MECA_POU_D_T_GD ','MECA_POU_D_TG   ',&
     &            'MECA_BARRE      ',&
     &            'MECA_DIS_T_L    ','MECA_DIS_TR_L   ',&
     &            'MECA_DIS_T_N    ','MECA_DIS_TR_N   '/
!
!-----------------------------------------------------------------------
    call jemarq()
    tol = 1.0d+08 * r8prem()
!
!     RECUPERATION DU NOMBRE DE MAILLES
    call jelira(noma//'.NOMMAI', 'NOMUTI', nbmail)
!
!     RECUPERATION DE LA MODELISATION DES MAILLES
!     VERIFICATION : PAS DE MAILLES TARDIVES
    modmai = modele//'.MAILLE'
    call jeveuo(modmai, 'L', jdme)
!
    ligrmo = modele//'.MODELE    '
    modnem = ligrmo//'.NEMA'
    nbmtrd = 0
    call jeexin(modnem, ixnw)
    if (ixnw .ne. 0) call jelira(modnem, 'NMAXOC', nbmtrd)
    if (nbmtrd .ne. 0) then
        call utmess('F', 'MODELISA2_23')
    endif
!
!     RECUPERATION DE LA CARTE D'ORIENTATION DES ELEMENTS
    carte = caelem//'.CARORIEN  '
    cadesc = carte//'.DESC'
    cavale = carte//'.VALE'
    call jeexin(cadesc, iexcar)
    if (iexcar .eq. 0) then
        call utmess('F', 'MODELISA2_24')
    endif
!
    call jeveuo(cadesc, 'L', idesc)
    call jeveuo(cavale, 'L', ivale)
!
    iasmax = zi(idesc+1)
!
!    DETERMINATION DES RANGS DES COMPOSANTES DE LA GRANDEUR <CAORIE>
!        <ALPHA>  <BETA>  <GAMMA>
    kexnom = jexnom('&CATA.GD.NOMCMP','CAORIE')
    call jelira(kexnom, 'LONMAX', ncmpor)
    call jeveuo(kexnom, 'L', icaori)
    ialpha = 0
    ibeta = 0
    igamma = 0
    ialpha = indik8( zk8(icaori) , nomcmp(1) , 1 , ncmpor )
    ibeta = indik8( zk8(icaori) , nomcmp(2) , 1 , ncmpor )
    igamma = indik8( zk8(icaori) , nomcmp(3) , 1 , ncmpor )
    ntrouv = (ialpha.eq.0).or.(ibeta.eq.0).or.(igamma.eq.0)
    if (ntrouv) then
        call utmess('F', 'MODELISA2_25')
    endif
!     NOMBRE D'ENTIERS CODES DANS LA CARTE
    call dismoi('NB_EC', 'CAORIE', 'GRANDEUR', repi=nbec)
!
!     EXTENSION DE LA CARTE D'ORIENTATION DES ELEMENTS : CREATION DE
!     VECTEURS D'ADRESSES DES CARACTERISTIQUES POINTES PAR LE NUMERO
!     DE MAILLE
    call etenca(carte, ligrmo, iret)
    if (iret .ne. 0) then
        call utmess('F', 'MODELISA2_26', sk=carte)
    endif
!
    captma = carte//'.PTMA'
    call jeveuo(captma, 'L', iptma)
!
!     RECUPERATION DES NUMEROS DES TYPES ELEMENTS ADMISSIBLES
    do itel = 1, nbtel
        call jenonu(jexnom('&CATA.TE.NOMTE', nomele(itel)), ntyele(itel))
    end do
!
!     RECUPERATION DES 3 ANGLES NAUTIQUES POUR TOUS LES ELEMENTS DE
!     LA STRUCTURE DE TYPE :
!              - ELEMENTS DE POUTRE DROITE
!              - ELEMENTS DE BARRE
!              - ELEMENTS DISCRETS DE LIAISON
!     AU PASSAGE ON VERIFIE QUE TOUS LES ELEMENTS SONT D'UN TYPE
!     ADMISSIBLE
    call wkvect('&&AXDIPO.TEMP.ANGL', 'V V R', 3*nbmail, iangl)
    pobali = 0
!
    do nummai = 1, nbmail
        nutyel = zi(jdme+nummai-1)
        itelok = 0
        do itel = 1, nbtel
            if (nutyel .eq. ntyele(itel)) then
                itelok = itel
                goto 32
            endif
        end do
        call utmess('F', 'MODELISA2_27')
 32     continue
!
        if (itelok .le. nbtel1) then
            ias = zi(iptma+nummai-1)
            if (ias .eq. 0) then
                call utmess('F', 'MODELISA2_28', si=nummai)
            endif
!
            icode = zi(idesc-1+3+2*iasmax+nbec*(ias-1)+1)
            irana = 0
            do icmp = 1, ialpha
                if (exisdg([icode],icmp)) irana = irana + 1
            end do
            iranb = 0
            do icmp = 1, ibeta
                if (exisdg([icode],icmp)) iranb = iranb + 1
            end do
            irang = 0
            do icmp = 1, igamma
                if (exisdg([icode],icmp)) irang = irang + 1
            end do
            ntrouv = (irana.eq.0).or.(iranb.eq.0).or.(irang.eq.0)
            if (ntrouv) then
                call utmess('F', 'MODELISA2_29', si=nummai)
            else
                pobali = pobali + 1
                iaux1 = ivale+ncmpor*(ias-1)
                zr(iangl+3*(pobali-1) ) = zr(iaux1+irana-1)
                zr(iangl+3*(pobali-1)+1) = zr(iaux1+iranb-1)
                zr(iangl+3*(pobali-1)+2) = zr(iaux1+irang-1)
            endif
        endif
!
    end do
    if (pobali .eq. 0) then
        call utmess('F', 'MODELISA2_30')
    endif
!
!     DETERMINATION DE L'AXE DIRECTEUR DE LA POUTRE A L'AIDE DES
!     ANGLES NAUTIQUES DU PREMIER ELEMENT ORIENTE : AFFECTATION DE IAXE
    vdl(1) = 1.d0
    vdl(2) = 0.d0
    vdl(3) = 0.d0
!
    alpha = zr(iangl)
    beta = zr(iangl+1)
    gamma = zr(iangl+2)
!
    sina = sin(alpha)
    cosa = cos(alpha)
    sinb = sin(beta)
    cosb = cos(beta)
    sing = sin(gamma)
    cosg = cos(gamma)
    call locglo(vdl, sina, cosa, sinb, cosb,&
                sing, cosg, vdg)
!
    vdg(1) = abs(vdg(1))
    vdg(2) = abs(vdg(2))
    vdg(3) = abs(vdg(3))
    dif1 = dble(abs(vdg(1) - 1.d0))
    dif2 = dble(abs(vdg(2) - 1.d0))
    dif3 = dble(abs(vdg(3) - 1.d0))
!
    iaxe = 0
    if (dif1 .lt. tol .and. vdg(2) .lt. tol .and. vdg(3) .lt. tol) then
        iaxe = 1
        else if (dif2.lt.tol .and. vdg(1).lt.tol .and. vdg(3).lt.tol)&
    then
        iaxe = 2
        else if (dif3.lt.tol .and. vdg(1).lt.tol .and. vdg(2).lt.tol)&
    then
        iaxe = 3
    endif
    if (iaxe .eq. 0) then
        call utmess('F', 'MODELISA2_31')
    endif
!
!     ON VERIFIE QUE LES ORIENTATIONS DES AUTRES ELEMENTS DEFINISSENT
!     LE MEME AXE DIRECTEUR
    if (pobali .gt. 1) then
        do ipbl = 2, pobali
            alpha2 = zr(iangl+3*(ipbl-1))
            beta2 = zr(iangl+3*(ipbl-1)+1)
            gamma2 = zr(iangl+3*(ipbl-1)+2)
            dif1 = dble(abs(alpha2 - alpha))
            dif2 = dble(abs(beta2 - beta ))
            dif3 = dble(abs(gamma2 - gamma))
            if (dif1 .gt. tol .or. dif2 .gt. tol .or. dif3 .gt. tol) then
                sina2 = sin(alpha2)
                cosa2 = cos(alpha2)
                sinb2 = sin(beta2)
                cosb2 = cos(beta2)
                sing2 = sin(gamma2)
                cosg2 = cos(gamma2)
                call locglo(vdl, sina2, cosa2, sinb2, cosb2,&
                            sing2, cosg2, vdg)
                vdg(1) = abs(vdg(1))
                vdg(2) = abs(vdg(2))
                vdg(3) = abs(vdg(3))
                dife1 = dble(abs(vdg(1) - 1.d0))
                dife2 = dble(abs(vdg(2) - 1.d0))
                dife3 = dble(abs(vdg(3) - 1.d0))
                iaxe2 = 0
                if (dife1 .lt. tol .and. vdg(2) .lt. tol .and. vdg(3) .lt. tol) then
                    iaxe2 = 1
                    else if (dife2.lt.tol .and. vdg(1).lt.tol .and. vdg(3)&
                .lt.tol) then
                    iaxe2 = 2
                    else if (dife3.lt.tol .and. vdg(1).lt.tol .and. vdg(2)&
                .lt.tol) then
                    iaxe2 = 3
                endif
                if (iaxe2 .ne. iaxe) then
                    call utmess('F', 'MODELISA2_32')
                endif
            endif
        end do
    endif
!
    call jedetr('&&AXDIPO.TEMP.ANGL')
    call jedetc('V', carte, 1)
!
    call jedema()
end subroutine
