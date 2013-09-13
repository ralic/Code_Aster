subroutine dglrdm()
    implicit none
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
! person_in_charge: sebastien.fayolle at edf.fr
! ----------------------------------------------------------------------
!
! BUT : DETERMINATION DES PARAMETRES MATERIAU POUR LE MODELE GLRC_DM
!
! IN:
!       LOI     : LOI DE COMPORTEMENT DE PLAQUE BETON ARME (GLRC_DM OU
!                 GLRC_DAMAGE)
!       IMATE   : ADRESSE DU MATERIAU (ACIER ET BETON)
!       COMPOR  : COMPORTMENT
!       EP      : EPAISSEUR DE LA PLAQUE
!       OMY     : SECTION D'ACIER D'UN LIT DE CABLES SUIVANT Y (M²/ML)
!       OMX     : SECTION D'ACIER D'UN LIT DE CABLES SUIVANT X (M²/ML)
!       RY      : POSITION ADIMENSIONNEE DU LIT DE CABLES SUIVANT Y
!       RX      : POSITION ADIMENSIONNEE DU LIT DE CABLES SUIVANT X
!       METHODE_ENDO : CHOIX DE LA METHODE DE CALCUL DE L'ENDOMMAGEMENT
!                 "ENDO_NAISS" : ON CONSIDERE UNE EVOLUTION
!                 INFINITESIMALE JUSTE APRES APPARITION DU PREMIER
!                 ENDOMMAGEMENT
!                 "ENDO_LIM"   : ON CONSIDERE DES ENDOMMAGMENTS
!                                IMPORTANTS
!                 "ENDO_INT"   : ON CALCULE LE RAPPORT
!                                PENTE D'ENDOMMAGEMENT/PENTE ELASTIQUE
!       PENTE   : METHODE RETENUE POUR CALCULER LA PENTE D'ENDOMMAGEMENT
!                 "ACIER_PLAS"     = RECALAGE A LA PLASTICITE DES ACIERS
!                 "UTIL" = RECALAGE A LA DEFO GENE MAX DE L'ELEMENT
!                 "RIGI_ACIER"= PENTE REPRISE DES RAIDEURS DES ACIERS
!       CISAIL  : RECALAGE PAR RAPPORT AU TEST DE CISAILLEMENT PUR
!       COMPR   : PARAMETRES D'ENDOMMAGEMENT INSERE PAR L'UTILISATEUR
!                 (GAMMA OU SEUIL D'ENDOMMAGEMENT)
!       INFO    : IMPRESSION DES PARAMETRES DE LA LOI GLRC_DM
! OUT:
!       RHO     : MASSE VOLUMIQUE DE LA STRUCTURE
!       AMORA   : AMORTISSEMENT ALPHA
!       AMORB   : AMORTISSEMENT BETA
!       EM      : PARAMETRE D ELASTICITE - MEMBRANE
!       NUM     : PARAMETRE D ELASTICITE - MEMBRANE
!       EF      : PARAMETRE D ELASTICITE - FLEXION
!       NUF     : PARAMETRE D ELASTICITE - FLEXION
!       GT      : PARAMETRE GAMMA POUR LA MEMBRANE EN TRACTION
!       GC      : PARAMETRE GAMMA POUR LA MEMBRANE EN COMPRESSION
!       GF      : PARAMETRE GAMMA POUR LA FLEXION
!       NYT     : SEUIL D'ENDOMMAGEMENT EN TRACTION
!       NYC     : SEUIL D'ENDOMMAGEMENT EN COMPRESSION
!       MYF     : SEUIL D'ENDOMMAGEMENT EN FLEXION
! ----------------------------------------------------------------------
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterfort/assert.h"
#include "asterfort/dgelas.h"
#include "asterfort/dgendo.h"
#include "asterfort/dgplas.h"
#include "asterfort/dgseui.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/rcvale.h"
#include "asterfort/ulexis.h"
#include "asterfort/ulopen.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    integer :: na
    parameter (na=10)
    integer :: nnap, ibid, ilit, jlm, jmelk
    integer :: jmelr, jmelc, lonobj
    integer :: icompr, iret, icisai
    integer :: ibid1, ibid2, ibid3
    integer :: nimpr, impr, ifr, iendo, ipente
!
    real(kind=8) :: ea(3*na), sya(3*na), eb, nub, sytb, b, b1, a
    real(kind=8) :: h, np, emaxm, emaxf
    real(kind=8) :: pendt, pendf, nyc, gt, gf, gc
    real(kind=8) :: num, nuf, em, ef, nyt, dxd, myf, dxp, drp, mp, rho, drd
    real(kind=8) :: pelast, pelasf, amora, amorb, rhob, rhoa, alpha, beta
    real(kind=8) :: omx(3*na), omy(3*na)
    real(kind=8) :: rx(3*na), ry(3*na)
    real(kind=8) :: valres(5), r8b
!
    integer :: icodr2(5)
    character(len=8) :: mater, k8b, compr, nomres(5)
    character(len=19) :: mendom
    character(len=19) :: cisail, pente
    character(len=16) :: type, nomcmd, fichie
!
    call jemarq()
!
    a=0.d0
    b=0.d0
    b1=0.d0
    gc=0.d0
    nyc=0.d0
!
! - DEFINITION DU NOMBRE DE NAPPE DANS L'EPAISSEUR
    call getfac('NAPPE', nnap)
    if (nnap .ne. 1) then
        call utmess('A', 'ALGORITH6_7')
    endif
!
! - VARIABLE D IMPRESSION DES PARAMETRES GLRC_DM
    nimpr = 0
    call getvis(' ', 'INFO', scal=impr, nbret=ibid1)
    if (impr .eq. 2) then
        nimpr = 1
        ifr = 8
        fichie = ' '
        if (.not. ulexis( impr )) then
            call ulopen(impr, ' ', fichie, 'NEW', 'O')
        endif
    endif
!
! - RELEVE DES CARACTERISTIQUES DU BETON
    call getvid('BETON', 'MATER', iocc=1, scal=mater, nbret=ibid)
    nomres(1) = 'E'
    nomres(2) = 'NU'
    nomres(3) = 'RHO'
    nomres(4) = 'AMOR_ALP'
    nomres(5) = 'AMOR_BET'
    k8b = ' '
    call getvr8('BETON', 'EPAIS', iocc=1, scal=h, nbret=ibid)
    r8b = 0.d0
    call rcvale(mater, 'ELAS            ', 0, k8b, [r8b],&
                5, nomres, valres, icodr2, 0)
    if (icodr2(1) .ne. 0 .or. icodr2(2) .ne. 0) then
        call utmess('A', 'ALGORITH6_8')
    endif
!
    eb = valres(1)
    nub = valres(2)
    rhob = valres(3)
!
    if (icodr2(4) .ne. 0) then
        amora = 0.0d0
    else
        amora = valres(4)
    endif
    if (icodr2(5) .ne. 0) then
        amorb = 0.0d0
    else
        amorb = valres(5)
    endif
!
    nomres(1) = 'SYT'
    call rcvale(mater, 'BETON_ECRO_LINE ', 0, k8b, [r8b],&
                1, nomres, valres, icodr2, 0)
    if (icodr2(1) .ne. 0) then
        call utmess('A', 'ALGORITH6_9')
    endif
    sytb = valres(1)
!
! - RECUPERATION DU PARAMETRE DE COMPRESSION SAISI PAR L'UTILISATEUR
    call getvtx(' ', 'COMPR', scal=compr, nbret=ibid2)
    if (compr .eq. 'GAMMA') then
        call getvr8(' ', 'GAMMA_C', scal=gc, nbret=ibid1)
    else if (compr .eq. 'SEUIL') then
        call getvr8(' ', 'NYC', scal=nyc, nbret=ibid1)
    endif
!
! - CARACTERISATION DES PARAMETRES DE COMPRESSION ENTRES PAR
!   L'UTILISATEUR
    if (nyc .ne. 0.0d0) then
        icompr=1
    else if (gc .ne. 0.0d0) then
        icompr=2
    else
        ASSERT(.false.)
    endif
! DEFINITION DES PROPRIETES MECANIQUE DU FERRAILLAGE
!      IF(NNAP .GT. 0) THEN
!        DO 10, ILIT = 1,NNAP
    ilit = 1
    call getvid('NAPPE', 'MATER', iocc=ilit, scal=mater, nbret=ibid)
    nomres(1) = 'E'
    nomres(2) = 'NU'
    nomres(3) = 'RHO'
!          NOMRES(4) = 'AMOR_ALPHA'
!          NOMRES(5) = 'AMOR_BETA'
    call rcvale(mater, 'ELAS            ', 0, k8b, [r8b],&
                3, nomres, valres, icodr2, 0)
!
    if (icodr2(1) .ne. 0 .or. icodr2(2) .ne. 0) then
        call utmess('A', 'ALGORITH6_10')
    endif
    ea(ilit) = valres(1)
!         NUA(ILIT)=VALRES(2) ON NE PREND PAS EN COMPTE L EFFET DE
!         POISSON SUR LES ARMATURES
    rhoa = valres(3)
!          IF(ICODR2(4) .NE. 0) THEN
!            AMORA = 0.0D0
!          ELSE
!            AMORA = VALRES(4)
!          ENDIF
!          IF(ICODR2(5) .NE. 0) THEN
!            AMORB = 0.0D0
!          ELSE
!            AMORB = VALRES(5)
!          ENDIF
    nomres(1) = 'SY'
    call rcvale(mater, 'ECRO_LINE       ', 0, k8b, [r8b],&
                1, nomres, valres, icodr2, 0)
    if (icodr2(1) .eq. 0) then
        sya(ilit) = valres(1)
    else
        sya(ilit) = -1.d0
    endif
!
    call getvr8('NAPPE', 'OMX', iocc=ilit, scal=omx(ilit), nbret=ibid)
    call getvr8('NAPPE', 'OMY', iocc=ilit, scal=omy(ilit), nbret=ibid)
    call getvr8('NAPPE', 'RX', iocc=ilit, scal=rx(ilit), nbret=ibid)
    call getvr8('NAPPE', 'RY', iocc=ilit, scal=ry(ilit), nbret=ibid)
    if ((omx(ilit) .ne. omy(ilit)) .or. (rx(ilit) .ne. ry(ilit))) then
        call utmess('A', 'ALGORITH6_6')
    endif
! Mise en coh�rence avec GLRC_DAMAGE
! D�veloppement fait pour -0.5<RX<0.5
! or pour la coh�rence avec GLRC_DAMAGE
! on se met dans le cas -1<RX<1
! pour repasser dans les conditions initiale on multiplie RX par 1/2
    rx(ilit)=rx(ilit)*0.5d0
    ry(ilit)=ry(ilit)*0.5d0
! Fin mise en coh�rence avec GLRC_DAMAGE
!
    b=ea(ilit)*(omx(ilit)+omy(ilit))
! B1=B1+EA(ILIT)*(RX(ILIT)+RY(ILIT))/2.*(OMX(ILIT)+OMY(ILIT))
! B1 = 0 du � la sym�trie de la plaque
    b1=0.d0
    a=ea(ilit)*(omx(ilit)+omy(ilit))*((rx(ilit)+ry(ilit))/2.d0)**2
! 10     CONTINUE
!      ENDIF
! RECUPARATION DE LA MASSE VOLUMIQUE EQUIVALENTE ET DES COEFFICIENTS
! D'AMORTISSEMENT DE RAYLEIGH
!
! ATTENTION CA NE FONCTIONNE PAS SI ON A PLUSIEURS ARMATURES
    call getvr8(' ', 'RHO', scal=rho, nbret=iret)
    if (iret .eq. 0) then
        rho=rhob + rhoa/h*2.d0*(omx(1)+omy(1))
    endif
    call getvr8(' ', 'AMOR_ALPHA', scal=alpha, nbret=ibid1)
    if (ibid1 .eq. 0) then
        alpha=amora
    endif
    call getvr8(' ', 'AMOR_BETA', scal=beta, nbret=ibid1)
    if (ibid1 .eq. 0) then
        beta=amorb
    endif
!
! RECUPERATION DES MOTS CLES "CISAIL", "METHODE_ENDO" et "PENTE"
    call getvtx(' ', 'PENTE', scal=pente, nbret=ibid1)
    if (pente .eq. 'UTIL') then
        ipente = 3
! - RECUPERATION DE LA DEFORMATION MAXIMALE EN MEMBRANE (EPSI_MAX_M)
! - RECUPERATION DE LA DEFORMATION MAXIMALE EN FLEXION (COUR_MAX_F)
        call getvr8(' ', 'EPSI_MEMB', scal=emaxm, nbret=ibid1)
        call getvr8(' ', 'KAPPA_FLEX', scal=emaxf, nbret=ibid1)
    else if (pente .eq. 'PLAS_ACIER') then
        if (sya(ilit) .le. 0.d0) then
            call utmess('F', 'ALGORITH6_11')
        endif
        ipente = 2
    else if (pente .eq. 'RIGI_ACIER') then
        ipente = 1
    endif
!
    call getvtx(' ', 'CISAIL', scal=cisail, nbret=ibid2)
    if (cisail .eq. 'OUI') then
        icisai = 1
    else
        icisai = 0
    endif
!
    call getvtx(' ', 'METHODE_ENDO', scal=mendom, nbret=ibid3)
    if (mendom(1:10) .eq. 'ENDO_NAISS') then
        iendo=1
    else if (mendom(1:8) .eq. 'ENDO_LIM') then
        iendo=2
    else if (mendom(1:10) .eq. 'ENDO_INTER') then
        iendo=3
    endif
! - CALCUL DES PARAMETRES ELASTIQUE HOMOGENEISES EM,NUM,EF,NUF
    call getres(mater, type, nomcmd)
    call dgelas(eb, nub, h, b, a,&
                em, num, ef, nuf, icisai)
! - DETERMINATION DES POINTS DE FISSURATION (DXD,NYT) ET (DRD,MYF)
!   ET DES PENTES ELASTIQUES
    call dgseui(em, num, ef, nuf, eb,&
                nub, sytb, h, icisai, nyt,&
                nyc, dxd, myf, drd, pelast,&
                pelasf, icompr)
! - DETERMINATION DES PENTES POST ELASTIQUE
    call dgplas(ea, sya, eb, nub, sytb,&
                num, nuf, a, b1, b,&
                nyt, myf, dxd, drd, h,&
                ipente, icisai, emaxm, emaxf, nnap,&
                rx, ry, np, dxp, pendt,&
                drp, mp, pendf)
! - DETERMINATION DES PARAMETRES D ENDOMMAGEMENT
    call dgendo(em, ef, h, nyt, nyc,&
                num, nuf, pendt, pelast, pendf,&
                pelasf, iendo, icisai, icompr, gt,&
                gf, gc, ipente, np, dxp)
!-----REMPLISSAGE DU MATERIAU
    call wkvect(mater//'.MATERIAU.NOMRC ', 'G V K16', 2, jlm)
    zk16(jlm ) = 'GLRC_DM         '
    zk16(jlm+1) = 'ELAS            '
!---------ELASTIQUE---------------
    lonobj = 5
    call wkvect(mater//'.ELAS      .VALK', 'G V K8', 2*lonobj, jmelk)
    call jeecra(mater//'.ELAS      .VALK', 'LONUTI', lonobj)
    call wkvect(mater//'.ELAS      .VALR', 'G V R', lonobj, jmelr)
    call jeecra(mater//'.ELAS      .VALR', 'LONUTI', lonobj)
    call wkvect(mater//'.ELAS      .VALC', 'G V C', lonobj, jmelc)
    call jeecra(mater//'.ELAS      .VALC', 'LONUTI', 0)
    zk8(jmelk ) = 'E       '
    zr(jmelr ) = em
    zk8(jmelk+1) = 'NU      '
    zr(jmelr+1 ) = num
    zk8(jmelk+2) = 'RHO     '
    zr(jmelr+2 ) = rho
    if (amora .gt. 0.0d0) then
        zk8(jmelk+3) = 'AMOR_ALP'
        zr(jmelr+3 ) = alpha
    endif
    if (amorb .gt. 0.0d0) then
        zk8(jmelk+4) = 'AMOR_BET'
        zr(jmelr+4 ) = beta
    endif
!---------GLRC_DM---------------
    lonobj = 10
    call wkvect(mater//'.GLRC_DM   .VALK', 'G V K8', 2*lonobj, jmelk)
    call jeecra(mater//'.GLRC_DM   .VALK', 'LONUTI', lonobj, ' ')
    call wkvect(mater//'.GLRC_DM   .VALR', 'G V R', lonobj, jmelr)
    call jeecra(mater//'.GLRC_DM   .VALR', 'LONUTI', lonobj, ' ')
    call wkvect(mater//'.GLRC_DM   .VALC', 'G V C', lonobj, jmelc)
    call jeecra(mater//'.GLRC_DM   .VALC', 'LONUTI', 0, ' ')
    zk8(jmelk ) = 'EF      '
    zr(jmelr ) = ef
    zk8(jmelk+1) = 'NUF     '
    zr(jmelr+1 ) = nuf
    zk8(jmelk+2) = 'EPAIS   '
    zr(jmelr+2) = h
    zk8(jmelk+3) = 'GAMMA_T '
    zr(jmelr+3 ) = gt
    zk8(jmelk+4) = 'GAMMA_F '
    zr(jmelr+4 ) = gf
    zk8(jmelk+5) = 'GAMMA_C '
    zr(jmelr+5 ) = gc
    zk8(jmelk+6) = 'NYT     '
    zr(jmelr+6 ) = nyt
    zk8(jmelk+7) = 'MYF     '
    zr(jmelr+7 ) = myf
    zk8(jmelk+8) = 'NYC     '
    zr(jmelr+8 ) = nyc
    zk8(jmelk+9) = 'ALPHA_C '
    zr(jmelr+9 ) = 1.d0
!---------IMPRESSION-------------
    if (nimpr .gt. 0) then
        write (ifr,*)
        write (ifr,*)
        write (ifr,*) 'PARAMETRES HOMOGENEISES POUR GLRC_DM :'
        write (ifr,*) 'PENTE = :',pente
        write (ifr,*) 'METHODE_ENDO = :',mendom
        write (ifr,*) 'CISAILLEMENT = :',cisail
        write (ifr,*) 'MODULE D YOUNG ET COEFFICIENT DE POISSON',&
        ' EFFECTIFS EN MEMBRANE:'
        write (ifr,*) 'EM =  :',em
        write (ifr,*) 'NUM =  :',num
        write (ifr,*) 'MODULE D YOUNG ET COEFFICIENT DE POISSON',&
        ' EFFECTIFS EN FLEXION:'
        write (ifr,*) 'EF =  :',ef
        write (ifr,*) 'NUF =  :',nuf
        write (ifr,*) 'LIMITES ELASTIQUES EN TRACTION, FLEXION ET',&
        ' COMPRESSION :'
        write (ifr,*) 'NYT =   :',nyt
        write (ifr,*) 'MYF =   :',myf
        write (ifr,*) 'NYC =   :',nyc
        write (ifr,*) 'PARAMETRES D ENDOMMAGEMENT:'
        write (ifr,*) 'GAMMA_T = ',gt
        write (ifr,*) 'GAMMA_F = ',gf
        write (ifr,*) 'GAMMA_C = ',gc
        write (ifr,*) 'ALPHA_C= ',1.d0
    endif
    call jedema()
end subroutine
