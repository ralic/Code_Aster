subroutine te0037(option, nomte)
    implicit none
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/conare.h"
#include "asterfort/confac.h"
#include "asterfort/elref1.h"
#include "asterfort/elref4.h"
#include "asterfort/fointe.h"
#include "asterfort/iselli.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jevecd.h"
#include "asterfort/jevech.h"
#include "asterfort/teattr.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
#include "asterfort/vecini.h"
#include "asterfort/xjacf2.h"
#include "asterfort/xjacff.h"
#include "asterfort/xteddl.h"
#include "asterfort/xteini.h"
#include "asterfort/xxmmvd.h"
!
    character(len=16) :: option, nomte
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
! person_in_charge: samuel.geniaut at edf.fr
!
!.......................................................................
!
!     BUT: CALCUL DES VECTEURS ELEMENTAIRES EN MECANIQUE
!          CORRESPONDANT A UN CHARGEMENT EN PRESSION REPARTIE
!          SUR LES LEVRES DES FISSURES X-FEM
!          (LA PRESSION PEUT ETRE DONNEE SOUS FORME D'UNE FONCTION)
!
!          OPTIONS : 'CHAR_MECA_PRES_R'
!                    'CHAR_MECA_PRES_F'
!
!  ENTREES  ---> OPTION : OPTION DE CALCUL
!           ---> NOMTE  : NOM DU TYPE ELEMENT
!
!.......................................................................
!
!
    character(len=8) :: elref, typma, fpg, elc, nompar(4), lag, elrefc
    integer :: ndim, nno, nnos, npg, ipoids, ivf, idfde, jgano
    integer :: nfh, nfe, singu, ddlc, nnom, ddls, nddl, ier, ddlm
    integer :: igeom, ipres, itemps, ires, iadzi, iazk24
    integer :: jlst, jptint, jaint, jcface, jlonch, jstno, jbasec, contac
    integer :: i, j, ninter, nface, cface(5, 3), ifa, nli, in(3), nfiss, jfisno
    integer :: ar(12, 3), nbar, fac(6, 4), nbf, ibid2(12, 3), ibid, cpt, ino
    integer :: ilev
    integer :: nnof, npgf, ipoidf, ivff, idfdef, ipgf, pos, zxain, nptf
    real(kind=8) :: mult, pres, cisa, forrep(3, 2), ff(27), jac, nd(3), he(2)
    real(kind=8) :: rr(2), lst, xg(4), dfbid(27, 3), r27bid(27), r3bid(3)
    logical :: lbid
    integer :: compt
    real(kind=8) :: thet
    data    he / -1.d0 , 1.d0/
!
    call jemarq()
!
!     PAR CONVENTION :
!     LEVRE INFERIEURE (HE=-1) EST LA LEVRE 1, DE NORMALE SORTANTE  ND
!     LEVRE SUPERIEURE (HE=+1) EST LA LEVRE 2, DE NORMALE SORTANTE -ND
!
!-----------------------------------------------------------------------
!     INITIALISATIONS
!-----------------------------------------------------------------------
    zxain = xxmmvd('ZXAIN')
!
    call elref1(elref)
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
!
!
!
!-----------------------------------------------------------------------
!     RECUPERATION DES ENTREES / SORTIE
!-----------------------------------------------------------------------
!
    if (option .eq. 'CHAR_MECA_PRES_R') then
!
!       SI LA PRESSION N'EST CONNUE SUR AUCUN NOEUD, ON LA PREND=0.
        call jevecd('PPRESSR', ipres, 0.d0)
        compt = 0
        do i = 1, nno
            thet = abs(zr(ipres-1+(i-1)+1))
            if (thet .lt. r8prem()) compt = compt + 1
        end do
        if (compt .eq. nno) goto 999
!
    else if (option.eq.'CHAR_MECA_PRES_F') then
!
        call jevech('PPRESSF', 'L', ipres)
        call jevech('PTEMPSR', 'L', itemps)
!
    else
        ASSERT(.false.)
    endif
!
    call jevech('PVECTUR', 'E', ires)
!
!
!
!
!     INITIALISATION DES DIMENSIONS DES DDLS X-FEM
    call xteini(nomte, nfh, nfe, singu, ddlc,&
                nnom, ddls, nddl, ddlm, nfiss,&
                contac)
!
    call tecael(iadzi, iazk24)
    typma=zk24(iazk24-1+3+zi(iadzi-1+2)+3)(1:8)
!
    if (ndim .eq. 3) then
        call confac(typma, ibid2, ibid, fac, nbf)
        elc='TR3'
        fpg='XCON'
    else if (ndim.eq.2) then
        call conare(typma, ar, nbar)
        if (iselli(elref)) then
            elc='SE2'
        else
            elc='SE3'
        endif
        fpg='MASS'
    endif
!
!
!
!
!     PARAMETRES PROPRES A X-FEM
    call jevech('PLST', 'L', jlst)
    call jevech('PPINTER', 'L', jptint)
    call jevech('PAINTER', 'L', jaint)
    call jevech('PCFACE', 'L', jcface)
    call jevech('PLONGCO', 'L', jlonch)
    call jevech('PSTANO', 'L', jstno)
    call jevech('PBASECO', 'L', jbasec)
    if (nfiss .gt. 1) call jevech('PFISNO', 'L', jfisno)
!
!     RÉCUPÉRATIONS DES DONNÉES SUR LA TOPOLOGIE DES FACETTES
    ninter=zi(jlonch-1+1)
    nface=zi(jlonch-1+2)
    nptf=zi(jlonch-1+3)
    if (ninter .lt. ndim) goto 999
!
    do i = 1, nface
        do j = 1, nptf
            cface(i,j)=zi(jcface-1+ndim*(i-1)+j)
        end do
    end do
!
    call jevech('PGEOMER', 'L', igeom)
!
!
!-----------------------------------------------------------------------
!     BOUCLE SUR LES FACETTES
!-----------------------------------------------------------------------
!
    do ifa = 1, nface
!
!       PETIT TRUC EN PLUS POUR LES FACES EN DOUBLE
        mult=1.d0
        do i = 1, ndim
            nli=cface(ifa,i)
            in(i)=nint(zr(jaint-1+zxain*(nli-1)+2))
        end do
!       SI LES 2/3 SOMMETS DE LA FACETTE SONT DES NOEUDS DE L'ELEMENT
        if (ndim .eq. 3) then
            if (in(1) .ne. 0 .and. in(2) .ne. 0 .and. in(3) .ne. 0) then
                do i = 1, nbf
                    cpt=0
                    do ino = 1, 4
                        if (in(1) .eq. fac(i,ino) .or. in(2) .eq. fac(i,ino) .or. in(3) .eq.&
                            fac(i,ino)) cpt=cpt+1
                    end do
                    if (cpt .eq. 3) then
                        mult=0.5d0
                        goto 104
                    endif
                end do
            endif
        else if (ndim .eq. 2) then
            if (in(1) .ne. 0 .and. in(2) .ne. 0) then
                do i = 1, nbar
                    cpt=0
                    do ino = 1, 2
                        if (in(1) .eq. ar(i,ino) .or. in(2) .eq. ar(i,ino)) cpt=cpt+1
                    end do
                    if (cpt .eq. 2) then
                        mult=0.5d0
                        goto 104
                    endif
                end do
            endif
        endif
104     continue
!
        call elref4(elc, fpg, ibid, nnof, ibid,&
                    npgf, ipoidf, ivff, idfdef, ibid)
!
!       BOUCLE SUR LES POINTS DE GAUSS DES FACETTES
        do ipgf = 1, npgf
!
!         CALCUL DE JAC (PRODUIT DU JACOBIEN ET DU POIDS)
!         ET DES FF DE L'ÉLÉMENT PARENT AU POINT DE GAUSS
!         ET LA NORMALE ND ORIENTÉE DE ESCL -> MAIT
!         ET DE XG : COORDONNEES REELLES DU POINT DE GAUSS
            elrefc='NON'
            if (ndim .eq. 3) then
                call xjacff(elref, elrefc, elc, ndim, fpg,&
                            jptint, ifa, cface, ipgf, nno,&
                            igeom, jbasec, xg, jac, ff,&
                            r27bid, dfbid, nd, r3bid, r3bid)
            else if (ndim.eq.2) then
                call xjacf2(elref, elrefc, elc, ndim, fpg,&
                            jptint, ifa, cface, nptf, ipgf,&
                            nno, igeom, jbasec, xg, jac,&
                            ff, r27bid, dfbid, nd, r3bid)
            endif
!
!         CALCUL DE RR = SQRT(DISTANCE AU FOND DE FISSURE)
            if (singu .eq. 1) then
                lst=0.d0
                do i = 1, nno
                    lst=lst+zr(jlst-1+i)*ff(i)
                end do
                ASSERT(lst.lt.0.d0)
                rr(1)=-sqrt(-lst)
                rr(2)= sqrt(-lst)
            endif
!
!
!         CALCUL DES FORCES REPARTIES SUIVANT LES OPTIONS
!         -----------------------------------------------
!
            call vecini(3*2, 0.d0, forrep)
            nompar(1)='X'
            nompar(2)='Y'
            if (ndim .eq. 3) nompar(3)='Z'
            if (ndim .eq. 3) nompar(4)='INST'
            if (ndim .eq. 2) nompar(3)='INST'
!
!
            if (option .eq. 'CHAR_MECA_PRES_R') then
!
!           CALCUL DE LA PRESSION AUX POINTS DE GAUSS
                pres = 0.d0
                cisa = 0.d0
                do ino = 1, nno
                    if (ndim .eq. 3) pres = pres + zr(ipres-1+ino) * ff( ino)
                    if (ndim .eq. 2) then
                        pres = pres + zr(ipres-1+2*(ino-1)+1) * ff( ino)
                        cisa = cisa + zr(ipres-1+2*(ino-1)+2) * ff( ino)
                    endif
                end do
!           ATTENTION AU SIGNE : POUR LES PRESSIONS, IL FAUT UN - DVT
!           CAR LE SECOND MEMBRE SERA ECRIT AVEC UN + (VOIR PLUS BAS)
!           ON CALCULE FORREP POUR LES DEUX LEVRES  : 1 = INF ET 2 = SUP
                do j = 1, ndim
                    forrep(j,1) = -pres * nd(j)
                    forrep(j,2) = -pres * (-nd(j))
                end do
                if (ndim .eq. 2) then
                    forrep(1,1) = forrep(1,1)- cisa * nd(2)
                    forrep(2,1) = forrep(2,1)+ cisa * nd(1)
                    forrep(1,2) = forrep(1,2)- cisa * (-nd(2))
                    forrep(2,2) = forrep(2,2)+ cisa * (-nd(1))
                endif
!
            else if (option.eq.'CHAR_MECA_PRES_F') then
!
!           VALEUR DE LA PRESSION
                xg(ndim+1) = zr(itemps)
                call fointe('FM', zk8(ipres), ndim+1, nompar, xg,&
                            pres, ier)
                if (ndim .eq. 2) call fointe('FM', zk8(ipres+1), ndim+1, nompar, xg,&
                                             cisa, ier)
                do j = 1, ndim
                    forrep(j,1) = -pres * nd(j)
                    forrep(j,2) = -pres * (-nd(j))
                end do
                if (ndim .eq. 2) then
                    forrep(1,1) = forrep(1,1)- cisa * nd(2)
                    forrep(2,1) = forrep(2,1)+ cisa * nd(1)
                    forrep(1,2) = forrep(1,2)- cisa * (-nd(2))
                    forrep(2,2) = forrep(2,2)+ cisa * (-nd(1))
                endif
            else
                call utmess('F', 'XFEM_15')
            endif
!
!         CALCUL EFFECTIF DU SECOND MEMBRE SUR LES DEUX LEVRES
            do ilev = 1, 2
!
                pos=0
                do ino = 1, nno
!
!             TERME CLASSIQUE
                    do j = 1, ndim
                        pos=pos+1
                        zr(ires-1+pos) = zr(ires-1+pos) + forrep(j, ilev) * jac * ff(ino) * mult
                    end do
!
!             TERME HEAVISIDE
                    do j = 1, nfh*ndim
                        pos=pos+1
                        zr(ires-1+pos) = zr(ires-1+pos) + he(ilev) * forrep(j,ilev) * jac * ff(in&
                                         &o) * mult
                    end do
!
!             TERME SINGULIER
                    do j = 1, singu*ndim
                        pos=pos+1
                        zr(ires-1+pos) = zr(ires-1+pos) + rr(ilev) * forrep(j,ilev) * jac * ff(in&
                                         &o) * mult
                    end do
!
!             ON SAUTE LES POSITIONS DES DDLS ASYMPTOTIQUES E2, E3, E4
                    pos = pos + (nfe-1) * ndim * singu
!
!             ON SAUTE LES POSITIONS DES LAG DE CONTACT FROTTEMENT
!
                    if (contac .eq. 3) then
                        if (ino .le. nnos) pos = pos + ddlc
                    else
                        pos = pos + ddlc
                    endif
!
                end do
!
            end do
!
        end do
    end do
!
!     SUPPRESSION DES DDLS SUPERFLUS
    call teattr(nomte, 'C', 'XLAG', lag, ibid)
    if (ibid .eq. 0 .and. lag .eq. 'ARETE') then
        nno = nnos
    endif
    call xteddl(ndim, nfh, nfe, ddls, nddl,&
                nno, nnos, zi(jstno), .false., lbid,&
                option, nomte, ddlm,&
                nfiss, jfisno, vect=zr(ires))
!
!
999 continue
    call jedema()
end subroutine
