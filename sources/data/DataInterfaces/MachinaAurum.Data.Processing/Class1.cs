using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace MachinaAurum.Data.Processing
{
    public interface IFunc
    {

    }

    public interface IConvexFunc : IFunc
    {

    }

    public interface ISet
    {

    }

    public interface IDataSet : ISet
    {

    }

    public interface IConvexSet : ISet
    {

    }


    public interface ITransformation : IFunc
    {

    }

    public interface IBinaryTransformation : ITransformation
    {

    }

    public interface IClosureBinaryTransformation<T> : IBinaryTransformation
    {
        T Run(T l, T r);
    }

    public interface IConvexFunctionPreserveTransformation : IClosureBinaryTransformation<IConvexFunc>
    {

    }

    public interface INonNegativeWeigthedSum : IConvexFunctionPreserveTransformation
    {

    }

    public interface IMaximumAdnSupremum : IConvexFunctionPreserveTransformation
    {

    }

    public interface IMaximumAdnSupremum : IConvexFunctionPreserveTransformation
    {

    }

    public interface IConvexSetPreserveTransformation : IClosureBinaryTransformation<IConvexSet>
    {

    }

    public interface IIntersection : IConvexSetPreserveTransformation
    {

    }

    public interface IAffineTransformation : IConvexSetPreserveTransformation
    {

    }

    public interface IPerspectiveTransformation : IConvexSetPreserveTransformation
    {

    }

    public interface IDimensionReduction : ITransformation
    {

    }

   

    public interface IPCADimensionReduction : IDimensionReduction
    {

    }

    public interface ILDADimensionReduction : IDimensionReduction
    {

    }

    public interface INumericalToCategorical : ITransformation
    {

    }

    public interface IBinning : INumericalToCategorical
    {

    }

    public interface IDiscretization : INumericalToCategorical
    {

    }

    public interface ICategoricalToNumerical : ITransformation
    {

    }

    public interface IEncoding : ICategoricalToNumerical
    {

    }

    public interface IContinuization : ICategoricalToNumerical
    {

    }

    public interface IClassifier : ITransformation
    {

    }

    public interface IFrequencyTable : ITransformation
    {

    }

    public interface IFrequencyTableClassifier : IClassifier
    {

    }

    public interface IZeroRClassifier : IFrequencyTableClassifier
    {

    }

    public interface IConfusionMatrix : ITransformation
    {

    }

    public interface IOneRClassifier : IFrequencyTableClassifier
    {

    }

    public interface INaiveBayesianClassifier : IFrequencyTableClassifier
    {

    }

    public interface IDecisionTreeClassifier : IFrequencyTableClassifier
    {

    }

    public interface ICovarianceMatrixClassifier : IClassifier
    {

    }

    public interface ILinearDiscriminantAnalysisClassifier : ICovarianceMatrixClassifier
    {

    }

    public interface ILogisticRegression : ICovarianceMatrixClassifier
    {

    }

    public interface IDataSimilarityClassifier
    {

    }

    public interface IKNNClassifier : IDataSimilarityClassifier
    {

    }

    public interface IPerceptronClassifier : IClassifier
    {

    }
}
